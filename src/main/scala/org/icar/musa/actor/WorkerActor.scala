package org.icar.musa.actor

import akka.actor.{Actor, ActorLogging}
import org.icar.fol.{AssumptionSet, Entail}
import org.icar.musa.spec.ConcreteCapability


class WorkerActor(concrete_cap : ConcreteCapability,ass_set: AssumptionSet) extends Actor with ActorLogging {
  import context._

  init

  def init : Unit = {
    log.info("ready")

    context.system.eventStream.subscribe(self,classOf[StateUpdate])

  }


  def free : Receive = {
    case "join" =>
      log.info("joined")
      concrete_cap.init
      become(ready)
  }

  def ready : Receive = {
    case "go" =>
      concrete_cap.pre_start
      become(waiting_pre_conditions)

    case "leave" =>
      concrete_cap.terminate
      become(free)

  }

  def waiting_pre_conditions : Receive = {
    case StateUpdate( w ) =>
      if (Entail.condition(w,ass_set,concrete_cap.abs_cap.pre)) {
        concrete_cap.execute
        become(waiting_post_conditions)
      }

    case "leave" =>
      concrete_cap.terminate
      become(free)

  }

  def waiting_post_conditions : Receive = {
    case StateUpdate( w ) =>
      if (Entail.condition(w,ass_set,concrete_cap.abs_cap.post)) {
        become(ready)
      }

    case "leave" =>
      concrete_cap.terminate
      become(free)

  }

  override def receive: Receive = free
}

