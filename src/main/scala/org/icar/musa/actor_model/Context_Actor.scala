package org.icar.musa.actor_model

import akka.actor.{Actor, ActorLogging, ActorSystem}
import org.icar.musa.context.{EnvContext, EvoOperator, Measurables, StateOfWorld}
import org.icar.musa.specification.DomainLoader

import scala.concurrent.duration._

class Context_Actor (domain : DomainLoader,starting_environment:EnvContext) extends Actor with ActorLogging {
  case class UpdateContext_Goal()

  val update_delay = 1 seconds
  var environment_context: EnvContext = new EnvContext


  override def preStart : Unit = {
    log.info("ready")

    environment_context.state_of_world = StateOfWorld.extend(starting_environment.state_of_world,domain.initial_state)
    environment_context.measurables = starting_environment.measurables

    // here create monitor agent for each env-variable
    // ...

    self ! UpdateContext_Goal()
  }

  override def receive: Receive = {

    case SimulatedStateUpdate(evo_scn) =>
      val ops: Array[EvoOperator] = evo_scn.evo
      environment_context.state_of_world = StateOfWorld.extend(environment_context.state_of_world,ops)
      log.debug("new state = "+environment_context.state_of_world.toString)

    case MeasurablesUpdate(m : Measurables) =>
      for (name <- m.variables) {
        if (environment_context.measurables.hasVariable(name))
          environment_context.measurables.updateVariable(name,m.getVariableValue(name))
        else
          environment_context.measurables.registerVariable(name,m.getVariableValue(name))
      }

    case UpdateContext_Goal() =>
      log.debug("inform_state")
      context.parent ! ContextUpdate(environment_context)

      import system.dispatcher
      val system : ActorSystem = ActorSystem("MUSA")
      system.scheduler.scheduleOnce(update_delay, self, UpdateContext_Goal())
  }

}
