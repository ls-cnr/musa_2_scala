package org.icar.actor_model

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import org.icar.pmr_solver.high_level_specification.{AvailableActions, Domain, LTLGoalSet}
import org.icar.pmr_solver.symbolic_level.RawState

import scala.org.icar.high_level_specification.Solution

class MeansEndMng(domain:Domain, goal_model:LTLGoalSet, available_actions:AvailableActions, initial_state:RawState) extends Actor with ActorLogging {
	var solution_map : Map[RawState,Array[Solution]] = Map.empty

	val my_solution_builder_actor : ActorRef = init_my_solution_builder
	val my_validator_actor : Option[ActorRef] = init_my_validator

	final override def receive: Receive = ???

	def init_my_solution_builder: ActorRef = {
		val props = Props.create(classOf[PlannerMng],domain,goal_model,available_actions,initial_state)
		context.actorOf(props)
	}
	def init_my_validator: Option[ActorRef] = None

}
