package org.icar.actor_model

import akka.actor.{Actor, ActorLogging, ActorRef}

abstract class OrchestrationMng extends Actor with ActorLogging {
	var workers : List[ActorRef] = init_my_workers

	final override def receive: Receive = {
		case _=>
	}

	def init_my_workers: List[ActorRef]

}
