package org.icar.musa.actor_model

import akka.actor.{Actor, ActorLogging, ActorSystem}
import org.icar.musa.context.Measurables
import org.icar.musa.specification.StateMonitorCapability


class Monitor_Actor(mon : StateMonitorCapability) extends Actor with ActorLogging {

  case class Monitor_The_State_Goal()

  val system = ActorSystem("MUSA")
  implicit val executionContext = system.dispatcher

  var measurables : Measurables = null


  override def preStart = {
    self ! Monitor_The_State_Goal()
  }

  override def receive: Receive = {
    case Monitor_The_State_Goal() =>
      log.info("checking the state for "+mon.name)
      if (measurables != null) {
        val evo = mon.check_state(measurables)
        context.parent ! SimulatedStateUpdate(evo)
      }

      system.scheduler.scheduleOnce(mon.delay, self, Monitor_The_State_Goal() )


    case ContextUpdate(environment_context) =>
      measurables = environment_context.measurables

  }
}
