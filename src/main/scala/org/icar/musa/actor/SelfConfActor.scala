package org.icar.musa.actor

import akka.actor.{Actor, ActorLogging, ActorSystem}
import org.icar.musa.context.StateOfWorld
import org.icar.musa.pmr._
import org.icar.musa.scenarios.{PRINEntertainmentScenario, PRINWakeUpScenario}
import org.icar.musa.spec.AbstractCapability

import scala.concurrent.duration._


class SelfConfActor(ps : SingleGoalProblemSpecification, musa_db : DBInfo, domain_id : Int) extends Actor with ActorLogging {
  var cap_set : Array[AbstractCapability] = load_capabilities

  var wi_opt : Option[StateOfWorld] = None
  val explorer = new SingleGoalProblemExploration(ps, cap_set, self.path.name)
  var seq_builder : SequenceBuilder = null
  var wts: WTS = null

  // if strategy==single-solution
  var solution_builder = new SingleSolutionBuilder



  init

  override def preStart = {
    // only early strategy
    //context.system.eventStream.subscribe(self, classOf[StateUpdate])
  }


  def init : Unit = {


    log.info("ready")
  }


  override def receive : Receive = {

    case StateUpdate( w ) =>
      log.debug("received state")
      if (!wi_opt.isDefined)
        wi_opt = Some(w)

    case SelfConfigureRequest(wi) =>
      log.debug("received self-conf request")
      wi_opt = Some(wi)
      set_wts(wi)
      self ! "explore_solution"


    case "explore_solution" =>
      log.debug("iteration")
      explorer.execute_iteration
      val exp_opt = explorer.highest_expansion
      update_wts(exp_opt)


      if (solution_builder.solution.complete==true)
        context.system.eventStream.publish(SingleSolution(solution_builder.solution))

      else {
        val system = ActorSystem("MUSA")
        import system.dispatcher
        system.scheduler.scheduleOnce(10 milliseconds, self, "explore_solution")
      }

  }


  private def update_wts(exp_opt: Option[WTSExpansion]) = {
    if (exp_opt.isDefined) {
      val exp = exp_opt.get
      wts.addExpansion(exp)

      exp match {
        case x: SimpleWTSExpansion =>
          explorer.pick_expansion(x)

          seq_builder.deal_with_expansion(x) //ADD AGAIN

          if (!x.end.su.isAccepted)
            explorer.new_node(x.end)


        case x: MultiWTSExpansion =>
          explorer.pick_expansion(x)

          seq_builder.deal_with_multi_expansion(x) //ADD AGAIN

          for (e <- x.evo.values) {
            if (!e.su.isAccepted)
              explorer.new_node(e)

          }


        case _ =>
      }
    }

  }

  private def set_wts(wi: StateOfWorld) : Unit = {
    val su = explorer.su_builder.initialize(ps.goal.ltl, wi, ps.ass_set)
    val qos = ps.asset.evaluate_node(wi, su.distance_to_satisfaction)
    val root = WTSStateNode(wi, su, qos)

    seq_builder = new SequenceBuilder(root,solution_builder)
    explorer.to_visit = root :: explorer.to_visit
    wts = new WTS(root)
  }

  private def load_capabilities : Array[AbstractCapability] = {
    val sc = new PRINWakeUpScenario //PRINEntertainmentScenario
    sc.capabilities
  }

}
