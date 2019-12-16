package org.icar.actor_model.protocol

import org.icar.actor_model.Protocol
import org.icar.pmr_solver.high_level_specification.HL_GroundLiteral

package object ContextProtocol extends Protocol {
	import org.icar.actor_model.ProtocolPart
	import org.icar.pmr_solver.symbolic_level.{RawState, RawVar}

	def init_context_registration : ProtocolPart = ContextRegistration(get_id)
	def init_observation_registration : ProtocolPart = ObservationRegistration(get_id)

	//def observation(preds:List[HL_GroundLiteral]) : ProtocolPart = InformObservations(get_id,preds)
	//def context_update(current:RawState, distance:Float) : ProtocolPart = InformContextUpdate(get_id,current,distance)
	//def internal_update(log:RawVar) : ProtocolPart = InformInternalUpdate(get_id,log)

	case class ContextRegistration private(id:Long) extends ProtocolPart {
		def context_update(current:RawState, distance:Float) : ProtocolPart = InformContextUpdate(get_id,current,distance)
		def internal_update(log:RawVar) : ProtocolPart = InformInternalUpdate(get_id,log)
	}

	case class ObservationRegistration private(id:Long) extends ProtocolPart {
		def observation(preds:List[HL_GroundLiteral]) : ProtocolPart = InformObservations(get_id,preds)
	}

	case class InformObservations private(id:Long,preds:List[HL_GroundLiteral]) extends ProtocolPart
	case class InformContextUpdate private(id:Long,current:RawState,distance:Float) extends ProtocolPart
	case class InformInternalUpdate private(id:Long,log:RawVar) extends ProtocolPart
}
