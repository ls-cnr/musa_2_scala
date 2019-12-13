package org.icar.actor_model.protocol

import org.icar.actor_model.Protocol

package object AdaptationProtocol extends Protocol {

	import org.icar.actor_model.ProtocolPart
	import org.icar.pmr_solver.high_level_specification.HL_LTLFormula

	def goal_violation(violated_goals:Array[HL_LTLFormula]) : ProtocolPart = InformGoalViolation(get_id,violated_goals)
	def concrete_failure(abstract_id:String) : ProtocolPart = InformConcreteFailure(get_id,abstract_id)
	def solution_became_invalid() : ProtocolPart = InformSolutionBecameInvalid(get_id)

	case class InformGoalViolation private(id:Long,violated_goals:Array[HL_LTLFormula]) extends ProtocolPart

	case class InformConcreteFailure private(id:Long,abstract_id:String) extends ProtocolPart

	case class InformSolutionBecameInvalid private(id:Long) extends ProtocolPart

}
