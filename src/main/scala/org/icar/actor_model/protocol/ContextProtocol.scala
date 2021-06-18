package org.icar.actor_model.protocol

import org.icar.actor_model.core.Protocol
import org.icar.pmr_solver.high_level_specification.{HL_GroundLiteral, HL_LTLFormula}

package object ContextProtocol extends Protocol {
	import org.icar.actor_model.core.ProtocolPart
	import org.icar.pmr_solver.symbolic_level.{RawState, RawVar}

	def init_context_registration : ProtocolPart = ContextRegistration(get_id)
	def init_observation_registration : ProtocolPart = ObservationRegistration(get_id)

	case class ContextRegistration private(id:Long) extends ProtocolPart {
		def context_update(current:RawState, distance:Float) : ProtocolPart = InformContextUpdate(id,current,distance)
		def internal_update(log:RawVar) : ProtocolPart = InformInternalUpdate(id,log)
		def goal_violation(goals:Array[HL_LTLFormula]) : ProtocolPart = InformGoalViolation(id,goals)
	}

	case class ObservationRegistration private(id:Long) extends ProtocolPart {
		def observation(preds:List[HL_GroundLiteral]) : ProtocolPart = InformObservations(id,preds)
	}

	case class InformObservations private(id:Long,preds:List[HL_GroundLiteral]) extends ProtocolPart
	case class InformContextUpdate private(id:Long,current:RawState,distance:Float) extends ProtocolPart
	case class InformInternalUpdate private(id:Long,log:RawVar) extends ProtocolPart
	case class InformGoalViolation private(id:Long,goals:Array[HL_LTLFormula]) extends ProtocolPart
}
