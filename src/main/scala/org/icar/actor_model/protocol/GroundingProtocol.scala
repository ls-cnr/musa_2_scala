package org.icar.actor_model.protocol

import org.icar.actor_model.core.{Protocol, ProtocolPart}

import scala.org.icar.high_level_specification.Task

package object GroundingProtocol extends Protocol {
	def init(task:Task) : ProtocolPart = CallForProposals(get_id,task)

	trait WorkerAuctionProtocolPart extends ProtocolPart

	case class CallForProposals private(id:Long, task:Task) extends WorkerAuctionProtocolPart {
		def participate(concrete_id:String) : ProtocolPart = Proposal(this.id,task,concrete_id)
	}

	case class Proposal private(id:Long,task:Task,concrete_id:String) extends WorkerAuctionProtocolPart {
		def choose_as_winner : ProtocolPart = AssignTask(this.id,task,concrete_id)
	}
	case class AssignTask private(id:Long,task:Task,concrete_id:String) extends WorkerAuctionProtocolPart
}

