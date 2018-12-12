package org.icar.musa.actor_model

import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, Props}
import org.icar.musa.context.{EnvContext, EvoOperator, Measurables, StateOfWorld}
import org.icar.musa.specification.{DomainLoader, StateMonitorCapability}

import scala.concurrent.duration._

class Context_Actor (domain : DomainLoader,starting_environment:EnvContext) extends Actor with ActorLogging {
  case class UpdateContext_Goal()

  val update_delay: FiniteDuration = 1 seconds
  var environment_context: EnvContext = new EnvContext

  var monitor_actors : List[ActorRef] = List()

  override def preStart : Unit = {
    log.info("ready")

    environment_context.state_of_world = StateOfWorld.extend(starting_environment.state_of_world,domain.initial_state)
    environment_context.measurables = starting_environment.measurables

    // here create monitor agent for each env-variable
    for (m<-domain.monitors)
      monitor_actors = create_monitor(m) :: monitor_actors

    self ! UpdateContext_Goal()
  }

  override def postStop(): Unit = {
    log.debug("completed")
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
      for (mon <- monitor_actors)
        mon ! ContextUpdate(environment_context)

      import system.dispatcher
      val system : ActorSystem = ActorSystem("MUSA")
      system.scheduler.scheduleOnce(update_delay, self, UpdateContext_Goal())
  }



  def create_monitor(m: StateMonitorCapability): ActorRef = {
    val props = Props.create(classOf[Monitor_Actor],m)
    val actor_name = m.name.toLowerCase+"mon"
    context.actorOf(props, actor_name)
  }


}
