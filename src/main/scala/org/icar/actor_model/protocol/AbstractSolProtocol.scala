package org.icar.actor_model.protocol

import org.icar.actor_model.{Protocol, ProtocolPart}
import org.icar.pmr_solver.high_level_specification.LTLGoalSet
import org.icar.pmr_solver.symbolic_level.RawState

import scala.org.icar.high_level_specification.Solution

package object AbstractSolProtocol extends Protocol {

	def init(initial_state:RawState,goal_set:LTLGoalSet) : ProtocolPart = RequestSolutions(get_id,initial_state,goal_set)

	case class RequestSolutions private(id:Long,initial_state:RawState,goal_set:LTLGoalSet) extends ProtocolPart {
		def forward_to_planner : ProtocolPart = RequestToPlan(this.id,initial_state,goal_set)
	}

	case class RequestToPlan private(id:Long,initial_state:RawState, goal_set:LTLGoalSet) extends ProtocolPart {
		def discovered_solutions(sol:Array[Solution]) : ProtocolPart = InformUnvalidatedPlans(this.id,initial_state,sol)
		def empty : ProtocolPart = InformEmptySet(this.id)
	}

	case class InformUnvalidatedPlans private(id:Long,initial_state:RawState,sol:Array[Solution]) extends ProtocolPart {
		def forward_to_validate() : ProtocolPart = RequestToValidatePlans(this.id,initial_state,sol)
		def accept_as_unvalidated() : ProtocolPart = InformSolutions(this.id,initial_state,sol)
	}
	case class InformEmptySet private(id:Long) extends ProtocolPart


	case class RequestToValidatePlans private(id:Long,initial_state:RawState,sol:Array[Solution]) extends ProtocolPart {
		def validated(subset:Array[Solution]) : ProtocolPart = InformSolutions(this.id,initial_state,subset)
	}
	case class InformSolutions private(id:Long,initial_state:RawState,sol:Array[Solution]) extends ProtocolPart

}
