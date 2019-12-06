package org.icar.actor_model

import akka.actor.{Actor, ActorLogging, ActorRef}
import org.icar.musa.specification.DomainLoader
import org.icar.pmr_solver.high_level_specification.AvailableActions

class DomainActor (domain : DomainLoader,available_actions:AvailableActions) extends Actor with ActorLogging {
	var session_registry : Map[String,ActorRef] = Map.empty

	override def receive: Receive = {
		case GoalMsg.NewSession(model) =>


		case GoalMsg.RemoveSession(id) =>


	}
}
