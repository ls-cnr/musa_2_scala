package org.icar.actor_model

import org.icar.pmr_solver.high_level_specification.{AvailableActions, Domain, LTLGoalSet}
import org.icar.pmr_solver.symbolic_level.RawState

import scala.org.icar.high_level_specification.Solution
import scala.org.icar.pmr_solver.best_first_planner.WTSGraph


object DomainMsg {
	case class Injection(domain:Domain,actions : AvailableActions)
}

object GoalMsg {
	case class NewSession(goal_model:LTLGoalSet)
	case class RemoveSession(session_id:String)
	case class Injection(session_id:String, goal_model:LTLGoalSet)
	case class Retreat(session_id:String, goal_model:LTLGoalSet)
}


object WorldMsg {
	case class Initial(raw_state:RawState, time_stamp: Long)
	case class Update(raw_state:RawState, time_stamp: Long)
}

object PlanningMsg {
	case class NoSolution()
	case class FullSolutionArray(sol:Array[Solution])
	case class PartialSolutionArray(sol:Array[Solution])
}