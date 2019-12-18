package org.icar.actor_model.protocol

import akka.actor.ActorRef
import org.icar.actor_model.core.{MetaSolInfo, Protocol, ProtocolPart}
import org.icar.pmr_solver.high_level_specification.LTLGoalSet
import org.icar.pmr_solver.symbolic_level.RawState

import scala.org.icar.high_level_specification.Solution

package object AbstractSolProtocol extends Protocol {

	def init(initial_state:RawState,goal_set:LTLGoalSet) : ProtocolPart = RequestSolutions(get_id,initial_state,goal_set)

	case class RequestSolutions private(id:Long,initial_state:RawState,goal_set:LTLGoalSet) extends ProtocolPart {
		def empty_sol_set : ProtocolPart = InformEmptySet(this.id)
		def forward_to_validate(reply_to:ActorRef,sol:Array[Solution]) : ProtocolPart = RequestToValidatePlans(this.id,reply_to,initial_state,sol)
		def accept_as_unvalidated(sol:Array[Solution]) : ProtocolPart = {
			val validated = for (s<-sol) yield MetaSolInfo(true, 0, s)
			InformSolutions(this.id,initial_state,validated)
		}
	}
	case class RequestToValidatePlans private(id:Long,reply_to:ActorRef,initial_state:RawState,sol:Array[Solution]) extends ProtocolPart {
		def empty_sol_set : ProtocolPart = InformEmptySet(this.id)
		def validated(subset:Array[MetaSolInfo]) : ProtocolPart = InformSolutions(this.id,initial_state,subset)
	}
	case class InformSolutions private(id:Long,initial_state:RawState,sol:Array[MetaSolInfo]) extends ProtocolPart
	case class InformEmptySet private(id:Long) extends ProtocolPart

}
