package org.icar.musa.actor_model

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import org.icar.musa.context.{DataIn, EnvContext, StateOfWorld}
import org.icar.musa.spec.{DomainLoader, SingleSession}

class Domain_Actor (domain : DomainLoader) extends Actor with ActorLogging {
  var session_counter : Int = 0
  var active_sessions : List[ActorRef] = List[ActorRef]()

  override def preStart : Unit = {
    log.info("ready for (id="+domain.name+")")

    domain.session_type match {
      case SingleSession() =>
        single_session_strategy

      case _ =>
        context.become(accepting_request)
    }

  }

  private def single_session_strategy = {
    val empty_env = new EnvContext
    val orchestrator_props = Props.create(classOf[Orchestrator_Actor], domain, empty_env)
    context.actorOf(orchestrator_props, "self-adaptive-orch")
  }

  override def receive: Receive = {
    case _ ⇒
  }

  def accepting_request : Receive = {
    case RequestNewSession(in : DataIn, wi : StateOfWorld) =>
      val env = new EnvContext
      env.measurables=in
      env.state_of_world= wi
      val orchestrator_props = Props.create(classOf[Orchestrator_Actor],domain,env)

      session_counter +=1
      context.actorOf(orchestrator_props, "self-adaptive-orch"+session_counter)

    case _ =>
  }

}
