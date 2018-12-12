package org.icar.musa.actor_model

import akka.actor.{Actor, ActorLogging, ActorSystem}
import org.icar.musa.context.StateOfWorld
import org.icar.musa.main_entity._
import org.icar.musa.pmr._
import org.icar.musa.specification.{EarlyDecisionWorkflow, DomainLoader, LateDecisionWorkflows, SolutionProperty}

import scala.concurrent.duration._


class SelfConf_Actor (domain : DomainLoader, wi: StateOfWorld) extends Actor with ActorLogging {

  /* internal goal specifications */
  case class ExploreSolutionSpaceGoal()

  /* subtasks specification */
  val explorer = new SingleGoalProblemExploration(problem_specification, cap_set, self.path.name)
  var solution_builder : AbstractSolutionBuilder = init_solution_builder(domain.solution_type)

  /* knowledge specification */
  val system = ActorSystem("MUSA")
  implicit val executionContext = system.dispatcher

  lazy val cap_set : Array[AbstractCapability] = domain.abstract_repository
  var wts: WTS = init_WTS(wi)

  var seq_builder : SequenceBuilder = init_sequence_builder(wts.root, solution_builder)

  var terminate = false

  var it_number = 0

  override def preStart : Unit = {
    /* single-workflow vs multiworkflow */
    domain.solution_type match {
      case EarlyDecisionWorkflow() =>
        log.info("ready [single solution]")
      case LateDecisionWorkflows() =>
        log.info("ready [many solutions]")
    }

    context.become(exploration)
    self ! ExploreSolutionSpaceGoal()

  }

  def exploration : Receive = {

    case ExploreSolutionSpaceGoal() =>
      if (terminate==false) {
        it_number += 1
        log.info("iteration "+it_number)
        explorer.execute_iteration
        val exp_opt = explorer.highest_expansion

        if (exp_opt.isDefined) {
          update_wts(exp_opt.get)

          //wts.print_for_graphviz(domain.quality_asset.pretty_string)

          check_complete_solutions
          //log.info("END iteration")

          system.scheduler.scheduleOnce(10 milliseconds, self, ExploreSolutionSpaceGoal() )
        } else {
          log.info("terminating because no expansion")
        }
      } else {
        log.info("terminated")
      }

    case TerminateSelfConfiguration() =>
      terminate = true

    case _ =>
  }

  override def receive: Receive = {
    case _ =>
  }


  /* task specifications */
  private def check_complete_solutions : Unit = {
    domain.solution_type match {
      case EarlyDecisionWorkflow() =>
        val single_solution_builder = solution_builder.asInstanceOf[LateDecisionSolutionBuilder]
        if (single_solution_builder.solution.complete==true) {
          log.info("complete")
          terminate=true

          context.parent ! SingleSolution(single_solution_builder.solution)
        }
      case LateDecisionWorkflows() =>
        val multi_solution_builder = solution_builder.asInstanceOf[EarlyDecisionSolutionBuilder]
        val n_p_s_s = multi_solution_builder.partial_solution_stack.size

        if (!multi_solution_builder.new_solutions.isEmpty) {
          //log.info("new solutions found")
          log.debug("complete solution size = "+multi_solution_builder.complete_solution.size)
          val set = multi_solution_builder.new_solutions.toSet

          //wts.print_for_graphviz(domain.quality_asset.pretty_string)

          context.parent ! MultiSolution(set) //context.system.eventStream.publish(MultiSolution(set))
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

  private def init_WTS(wi: StateOfWorld): WTS = {
    val su = explorer.su_builder.initialize(domain.goal.ltl, wi, domain.assumption )
    val qos = domain.quality_asset.evaluate_node(wi, su.distance_to_satisfaction)
    val root = WTSStateNode(wi, su, qos)

    seq_builder = new SequenceBuilder(root, solution_builder)
    explorer.to_visit = root :: explorer.to_visit
    new WTS(root)
  }

  private def init_solution_builder(solution_type: SolutionProperty): AbstractSolutionBuilder = {
    solution_type match {
      case EarlyDecisionWorkflow() => new LateDecisionSolutionBuilder
      case LateDecisionWorkflows() => new EarlyDecisionSolutionBuilder
    }
  }

  private def init_sequence_builder(root:WTSStateNode,solution_builder : AbstractSolutionBuilder): SequenceBuilder = {
    new SequenceBuilder(root,solution_builder)
  }

  private def problem_specification: SingleGoalProblemSpecification = {
    SingleGoalProblemSpecification(domain.assumption,domain.goal,domain.quality_asset)
  }
}
