package org.icar.musa.actor

import akka.actor.{Actor, ActorLogging, ActorSystem}
import org.icar.musa.context.{EvoOperator, StateOfWorld}
import org.icar.musa.scenarios.{PRINEntertainmentScenario, PRINWakeUpScenario}

import scala.concurrent.duration._


class ContextActor(musa_db : DBInfo, domain_id : Int) extends Actor with ActorLogging {
  var w : StateOfWorld = load_state

  val inform_delay = 1 seconds

  init

  def init : Unit = {
    log.info("ready")

    context.system.eventStream.subscribe(self,classOf[SimulatedStateUpdate])

    // here create monitor agent for each env-variable
    log.debug("initial state = "+w.toString)
    self ! "inform_state"
  }


  override def receive: Receive = {

    case SimulatedStateUpdate(evo_scn) =>
      val ops: Array[EvoOperator] = evo_scn.evo
      w = StateOfWorld.extend(w,ops)
      log.debug("new state = "+w.toString)

    case "inform_state" =>
      log.debug("inform_state")
      context.system.eventStream.publish(StateUpdate(w))

      import system.dispatcher
      val system : ActorSystem = ActorSystem("MUSA")
      system.scheduler.scheduleOnce(inform_delay, self, "inform_state")
  }



  private def load_state : StateOfWorld = {
    val sc = new PRINWakeUpScenario //PRINEntertainmentScenario
    sc.initial_state
  }

}
