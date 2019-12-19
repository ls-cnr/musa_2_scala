package org.icar.actor_model.core

import akka.actor.{Actor, ActorSystem}

import scala.concurrent.ExecutionContextExecutor

trait MUSAActor extends Actor {
	val system : ActorSystem = context.system
	implicit val executionContext: ExecutionContextExecutor = system.dispatcher

	var roles : List[Receive] = List(not_understood)
	protected def registerRole(receive: Receive) {
		roles = receive :: roles
	}

	private def not_understood : Receive = {
		case _ => NotUnderstood
	}

	def receive: Receive = roles reduce {_ orElse _}

	val my_log_area : MUSALogger

	def mylog(string:String) : Unit = {
		my_log_area.mylog(string)
	}
}

