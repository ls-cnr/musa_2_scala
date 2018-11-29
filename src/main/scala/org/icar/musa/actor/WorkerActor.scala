package org.icar.musa.actor

import akka.actor.{Actor, ActorLogging, ActorRef}
import org.icar.fol.{AssumptionSet, Entail}
import org.icar.musa.spec.ConcreteCapability


class WorkerActor(concrete_cap : ConcreteCapability,ass_set: AssumptionSet,recruiter:ActorRef) extends Actor with ActorLogging {
  import context._

  override def preStart : Unit = {
    log.info("ready")

    context.system.eventStream.subscribe(self,classOf[StateUpdate])
  }


  def free : Receive = {
    case "join" =>
      log.debug("joined")
      concrete_cap.init
      become(ready)
  }

  def ready : Receive = {
    case "go" =>
      log.debug("ready for "+concrete_cap.name)
      concrete_cap.pre_start
      become(waiting_pre_conditions)

    case "leave" =>
      concrete_cap.terminate
      become(free)

  }

  def waiting_pre_conditions : Receive = {
    case StateUpdate( w ) =>
      log.debug("checking pre-conditions for "+concrete_cap.name)
      if (Entail.condition(w,ass_set,concrete_cap.abs_cap.pre)) {

        concrete_cap.execute
        log.debug("executed "+concrete_cap.name+": "+concrete_cap.scn)

        val evo_opt = concrete_cap.get_simulated_scenario
        if (evo_opt.isDefined)
          context.system.eventStream.publish(SimulatedStateUpdate(evo_opt.get))

        become(waiting_post_conditions)
      }

    case "leave" =>
      concrete_cap.compensate
      become(free)

  }

  def waiting_post_conditions : Receive = {
    case StateUpdate( w ) =>
      log.debug("checking post-conditions for "+concrete_cap.name+" that is "+concrete_cap.abs_cap.post)
      if (Entail.condition(w,ass_set,concrete_cap.abs_cap.post)) {
        concrete_cap.post_end

        log.debug("task completed for "+recruiter.path)
        recruiter ! TaskCompleted(concrete_cap.abs_cap.name, concrete_cap.scn)

        become(ready)
      }

    case "leave" =>
      concrete_cap.compensate
      become(free)

  }

  override def receive: Receive = free
}

