package org.icar.musa.actor_model

import akka.actor.{Actor, ActorLogging, ActorSystem}
import org.icar.musa.context.Measurables
import org.icar.musa.specification.StateMonitorCapability

import scala.concurrent.ExecutionContextExecutor


class Monitor_Actor(mon : StateMonitorCapability) extends Actor with ActorLogging {

  case class Monitor_The_State_Goal()

  val system = ActorSystem("MUSA")
  implicit val executionContext: ExecutionContextExecutor = system.dispatcher

  var measurables : Measurables = null

  var count = 0


  override def preStart: Unit = {
    self ! Monitor_The_State_Goal()
  }

  override def receive: Receive = {
    case Monitor_The_State_Goal() =>
      count += 1
      if (count==20) {
        log.debug("checking the state for "+mon.name)
        count = 0
      }
      if (measurables != null) {
        val evo = mon.check_state(measurables)
        context.parent ! SimulatedStateUpdate(evo)
      }

      system.scheduler.scheduleOnce(mon.delay, self, Monitor_The_State_Goal() )


    case ContextUpdate(environment_context) =>
      measurables = environment_context.measurables
      log.debug("measurables"+measurables.toString)

  }
}
