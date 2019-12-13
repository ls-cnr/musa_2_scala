package org.icar.actor_model.protocol

import org.icar.actor_model.{Protocol, ProtocolPart}

import scala.org.icar.high_level_specification.Task

package object GroundingProtocol extends Protocol {
	def init(abstract_id:String) : ProtocolPart = CallForProposals(get_id,abstract_id)

	trait WorkerAuctionProtocolPart extends ProtocolPart

	case class CallForProposals private(id:Long, abstract_id:String) extends WorkerAuctionProtocolPart {
		def participate(concrete_id:String) : ProtocolPart = Proposal(this.id,abstract_id,concrete_id)
	}

	case class Proposal private(id:Long,abstract_id:String,concrete_id:String) extends WorkerAuctionProtocolPart {
		def choose_as_winner(task:Task) : ProtocolPart = AssignTask(this.id,task,concrete_id)
	}
	case class AssignTask private(id:Long,task:Task,concrete_id:String) extends WorkerAuctionProtocolPart {
		def task_progress(perc:Float) : ProtocolPart = InformTaskProgress(this.id,task,concrete_id,perc)
		def task_completed : ProtocolPart = InformCompletedTask(this.id,task,concrete_id)
		def task_failed : ProtocolPart = InformFailedTask(this.id,task,concrete_id)
	}
	case class InformTaskProgress private(id:Long, task:Task,concrete_id:String, perc:Float) extends WorkerAuctionProtocolPart
	case class InformCompletedTask private(id:Long, task:Task,concrete_id:String) extends WorkerAuctionProtocolPart
	case class InformFailedTask private(id:Long, task:Task,concrete_id:String) extends WorkerAuctionProtocolPart
}

