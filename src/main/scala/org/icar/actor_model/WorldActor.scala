package org.icar.actor_model

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import org.icar.pmr_solver.high_level_specification.{Domain, LTLGoalSet}
import org.icar.pmr_solver.symbolic_level.HL2Raw_Map

abstract class WorldActor(domain:Domain, goal_model:LTLGoalSet) extends Actor with ActorLogging {
	val raw_domain = new HL2Raw_Map(domain)

	var monitor_list : List[ActorRef] = init_monitors
	val runtime_checker : ActorRef = init_checker

	override def receive: Receive = {
		case _ =>
	}


	def init_monitors: List[ActorRef]

	private def init_checker: ActorRef = {
		val props = Props.create(classOf[ModelCheckerActor])
		context.actorOf(props)
	}

}
