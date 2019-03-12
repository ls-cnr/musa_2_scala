package org.icar.musa.actor_model

import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem, PoisonPill, Props}
import org.icar.fol.{AtomTerm, GroundPredicate}
import org.icar.musa.context.{DataIn, EnvContext, StateOfWorld}
import org.icar.musa.specification.{DomainLoader, NoProxyStrategy, ProxyCapability, SingleSession}

import scala.concurrent.ExecutionContextExecutor
import scala.concurrent.duration._

class Domain_Actor (domain : DomainLoader) extends Actor with ActorLogging {
  var session_counter : Int = 0
  var active_sessions : List[ActorRef] = List[ActorRef]()

  val proxy_strategy: ProxyCapability = domain.proxy_strategy.getOrElse(new NoProxyStrategy)

  val system = ActorSystem("MUSA")
  implicit val executionContext: ExecutionContextExecutor = system.dispatcher

  override def preStart : Unit = {
    log.info("ready for (id="+domain.name+")")

    domain.session_type match {
      case SingleSession() =>
        single_session_strategy

      case _ =>
        context.become(accepting_request)

        val thread = new Thread(proxy_strategy)
        proxy_strategy.set_callback(new_request)
        thread.start()


        /*val d1 = new DataIn()
        d1.registerVariable("document_id",153)
        val w1 = StateOfWorld.create(GroundPredicate("request", AtomTerm("id")),GroundPredicate("document", AtomTerm("id")))
        system.scheduler.scheduleOnce(10 milliseconds, self, RequestNewSession(d1,w1) )*/
    }

  }

  private def single_session_strategy = {
    val empty_env = new EnvContext
    //val orchestrator_props = Props.create(classOf[Orchestrator_Actor], domain, empty_env)
    //context.actorOf(orchestrator_props, "self-adaptive-orch")
    val adaptive_manager = Props.create(classOf[AdaptiveManager_Actor], domain, empty_env)
    context.actorOf(adaptive_manager, "adaptive-manager")
  }

  private def new_request(r:RequestNewSession): Unit = {
    self ! r
  }

  override def receive: Receive = {
    case SessionTerminated() =>
      active_sessions = active_sessions.filter( _  != sender )
      sender ! PoisonPill

    case _ ⇒
  }

  def accepting_request : Receive = {
    case RequestNewSession(in : DataIn, wi : StateOfWorld) =>
      val env = new EnvContext
      env.measurables=in
      env.state_of_world= wi

      session_counter +=1
      //val orchestrator_props = Props.create(classOf[Orchestrator_Actor],domain,env)
      //context.actorOf(orchestrator_props, "self-adaptive-orch"+session_counter)

      val adaptive_manager = Props.create(classOf[AdaptiveManager_Actor], domain, env)
      context.actorOf(adaptive_manager, "adaptive-manager"+session_counter)

    case _ =>
  }

}
