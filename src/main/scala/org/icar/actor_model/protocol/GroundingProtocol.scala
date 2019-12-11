package org.icar.actor_model.protocol

import org.icar.actor_model.{MessageContent, Protocol, ProtocolPart}

package object GroundingProtocol extends Protocol {
	def init(worker_number:Int) : ProtocolPart = CallForProposals(get_id,worker_number)

	trait WorkerAuctionProtocolPart extends ProtocolPart

	private case class CallForProposals(id:Long, auctionees:Int) extends WorkerAuctionProtocolPart {
		def participate() : ProtocolPart = Proposal(this.id)
	}
	private case class Proposal(id:Long) extends WorkerAuctionProtocolPart {
		def choose_as_winner() : ProtocolPart = AssignTask(this.id)
	}
	private case class AssignTask(id:Long) extends WorkerAuctionProtocolPart {
		def task_progress(perc:Float) : ProtocolPart = InformTaskProgress(this.id,perc)
		def task_completed : ProtocolPart = InformCompletedTask(this.id)
		def task_failed : ProtocolPart = InformFailedTask(this.id)
	}
	private case class InformTaskProgress(id:Long, perc:Float) extends WorkerAuctionProtocolPart
	private case class InformCompletedTask(id:Long) extends WorkerAuctionProtocolPart
	private case class InformFailedTask(id:Long) extends WorkerAuctionProtocolPart
}

