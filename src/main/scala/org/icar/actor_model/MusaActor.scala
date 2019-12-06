package org.icar.actor_model

//import akka.actor.typed.Behavior
//import akka.actor.typed.scaladsl.{AbstractBehavior, ActorContext, Behaviors}
import akka.actor.{Actor, ActorLogging, ActorRef, Props}


class MusaActor extends Actor with ActorLogging {
	var domain_registry : Map[String,ActorRef] = Map.empty

	override def receive: Receive = {
		case DomainMsg.Injection(domain,available_actions) =>

			val props = Props.create(classOf[DomainActor],domain,available_actions)
			val actor_name = domain.id.replace( ' ', '_')+"actor"
			val domain_actor : ActorRef = context.actorOf(props, actor_name)

			domain_registry += (domain.id -> domain_actor)
	}

}



/*
class MusaActor(context: ActorContext[DomainInjection]) extends AbstractBehavior[DomainInjection](context) {
	//var domain_registry : Map[String,ActorRef[DomainInjection]] = Map.empty


	override def onMessage(msg: DomainInjection): Behavior[DomainInjection] = {
		val domain = msg.domain
		val domain_actor = context.spawn(DomainActor(domain),domain.id+"actor")

		//domain_registry += (domain.id -> domain_actor)
		this
	}

}

object MusaActor {
	def apply() : Behavior[DomainInjection] =
		Behaviors.setup(context=>new MusaActor(context))
}
*/


