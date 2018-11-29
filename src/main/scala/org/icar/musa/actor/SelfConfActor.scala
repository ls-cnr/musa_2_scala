package org.icar.musa.actor

import akka.actor.{Actor, ActorLogging, ActorSystem}
import org.icar.musa.context.StateOfWorld
import org.icar.musa.pmr._
import org.icar.musa.spec._

import scala.concurrent.duration._


class SelfConfActor(domain : DomainLoader) extends Actor with ActorLogging {
  /* internal goal specifications */
  case class ExploreSolutionSpaceGoal()

  /* knowledge specification */
  var cap_set : Array[AbstractCapability] = load_capabilities
  var wi_opt : Option[StateOfWorld] = None
  var wts: WTS = _

  /* subtasks specification */
  val explorer = new SingleGoalProblemExploration(problem_specification, cap_set, self.path.name)
  var seq_builder : SequenceBuilder = _
  var solution_builder : AbstractSolutionBuilder = _

  /* initial configuration */
  override def preStart : Unit = {
    var mystate = ""
    /* single-workflow vs multiworkflow */
    domain.solution_type match {
      case AllInOneWorkflow() =>
        mystate += "single solution"
        solution_builder = new SingleSolutionBuilder
      case ManyAlternativeWorkflows() =>
        mystate += "many solutions"
        solution_builder = new MultiSolutionBuilder
    }

    /* early wts exploration vs late wts exploration */
    domain.wts_exploration_type match {
      case EarlyWTSExploration() =>
        mystate += " - early strategy"
        context.system.eventStream.subscribe(self, classOf[StateUpdate])
        context.become(araly_strategy)
      case _ =>
        mystate += " - ondemand strategy"
        context.become(ondemand_strategy)
    }

    log.info("ready ["+mystate+"]")

  }

  /* event loop specifications */
  def araly_strategy : Receive = {
    case StateUpdate( wi ) =>
      log.debug("received state")
      if (!wi_opt.isDefined) {
        wi_opt = Some(wi)
        set_wts(wi)
        context.become(exploration)
        self ! ExploreSolutionSpaceGoal()
      }
  }

  def ondemand_strategy : Receive = {
    case SelfConfigureRequest(wi) =>
      log.debug("received self-conf request")
      wi_opt = Some(wi)
      set_wts(wi)
      context.become(exploration)
      self ! ExploreSolutionSpaceGoal()
  }

  def exploration : Receive = {

    case ExploreSolutionSpaceGoal() =>
      log.info("iteration")
      explorer.execute_iteration
      val exp_opt = explorer.highest_expansion

      if (exp_opt.isDefined) {
        log.debug("expansion")
        update_wts(exp_opt.get)

        check_complete_solutions

        val system = ActorSystem("MUSA")
        import system.dispatcher
        system.scheduler.scheduleOnce(10 milliseconds, self, ExploreSolutionSpaceGoal() )
      }

  }

  override def receive: Receive = {
    case _ =>
  }


  /* task specifications */
  private def check_complete_solutions : Unit = {
    domain.solution_type match {
      case AllInOneWorkflow() =>
        val single_solution_builder = solution_builder.asInstanceOf[SingleSolutionBuilder]
        if (single_solution_builder.solution.complete==true) {
          log.debug("complete")
          context.system.eventStream.publish(SingleSolution(single_solution_builder.solution))
        }
      case ManyAlternativeWorkflows() =>
        val multi_solution_builder = solution_builder.asInstanceOf[MultiSolutionBuilder]
        if (!multi_solution_builder.new_solutions.isEmpty) {
          log.debug("partial results")
          val set = multi_solution_builder.new_solutions.toSet
          context.system.eventStream.publish(MultiSolution(set))
          multi_solution_builder.new_solutions = List()
        }
    }
  }


  private def update_wts(exp : WTSExpansion) = {
    wts.addExpansion(exp)

    exp match {
      case x : SimpleWTSExpansion =>
        explorer.pick_expansion(x)

        seq_builder.deal_with_expansion(x)

        if (!x.end.su.isAccepted)
          explorer.new_node(x.end)


      case x : MultiWTSExpansion =>
        explorer.pick_expansion(x)

        seq_builder.deal_with_multi_expansion(x)

        for (e <- x.evo.values) {
          if (!e.su.isAccepted)
            explorer.new_node(e)

        }

      case _ =>
    }

  }

  private def set_wts(wi: StateOfWorld) : Unit = {
    val su = explorer.su_builder.initialize(domain.goal.ltl, wi, domain.assumption )
    val qos = domain.quality_asset.evaluate_node(wi, su.distance_to_satisfaction)
    val root = WTSStateNode(wi, su, qos)

    seq_builder = new SequenceBuilder(root, solution_builder)
    explorer.to_visit = root :: explorer.to_visit
    wts = new WTS(root)
  }

  private def load_capabilities : Array[AbstractCapability] = {
    domain.abstract_repository
  }

  private def problem_specification: SingleGoalProblemSpecification = {
    SingleGoalProblemSpecification(domain.assumption,domain.goal,domain.quality_asset)
  }

}
