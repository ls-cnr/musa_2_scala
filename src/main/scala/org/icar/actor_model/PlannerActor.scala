package org.icar.actor_model

import akka.actor.{Actor, ActorLogging}
import org.icar.pmr_solver.high_level_specification.{AvailableActions, Domain, LTLGoalSet}
import org.icar.pmr_solver.symbolic_level.RawState

import scala.org.icar.pmr_solver.best_first_planner.Solver

class PlannerActor(domain:Domain, goal_model:LTLGoalSet, available_actions:AvailableActions, initial_state:RawState) extends Actor with ActorLogging {
	def qos(state:RawState) : Float = ???
	val solver = Solver.mixed_factory(goal_model,available_actions,initial_state,domain,qos)

	override def receive: Receive = ???
}
