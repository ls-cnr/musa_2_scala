package org.icar.musa.actor_model

import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, Props}
import org.icar.fol.{AtomTerm, GroundPredicate}
import org.icar.musa.context.{DataIn, EnvContext, StateOfWorld}
import org.icar.musa.spec.{DomainLoader, SingleSession}

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration._

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

        val d1 = new DataIn()
        d1.registerVariable("document_id",153)
        val w1 = StateOfWorld.create(GroundPredicate("request", AtomTerm("id")),GroundPredicate("document", AtomTerm("id")))
        val system = ActorSystem("MUSA")
        import system.dispatcher
        system.scheduler.scheduleOnce(10 milliseconds, self, RequestNewSession(d1,w1) )
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
