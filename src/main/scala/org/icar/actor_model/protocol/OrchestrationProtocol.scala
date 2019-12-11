package org.icar.actor_model.protocol

import org.icar.actor_model.{Protocol, ProtocolPart}

import scala.org.icar.high_level_specification.Solution

package object OrchestrationProtocol extends Protocol {
	def init(sol:Solution) : ProtocolPart = RequestApplySolution(get_id,sol)
	def run_time_change(conv_id:Long, sol:Solution) : ProtocolPart = RequestSwitchSolution(conv_id,sol)

	case class RequestApplySolution private(id:Long, sol:Solution) extends ProtocolPart {
		def not_enough_concrete : ProtocolPart = InformGroundingFailure(this.id,sol)
		def terminated : ProtocolPart = InformSolutionApplied(this.id,sol)
	}

	case class RequestSwitchSolution private(id:Long,sol:Solution) extends ProtocolPart {
		def not_enough_concrete : ProtocolPart = InformGroundingFailure(this.id,sol)
		def terminated : ProtocolPart = InformSolutionApplied(this.id,sol)
	}

	case class InformGroundingFailure private(id:Long,sol:Solution) extends ProtocolPart

	case class InformSolutionApplied private(id:Long,sol:Solution) extends ProtocolPart

}
