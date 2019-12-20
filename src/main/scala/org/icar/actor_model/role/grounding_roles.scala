package org.icar.actor_model.role

import akka.actor.ActorRef
import org.icar.actor_model.core.{MUSARole, ProtocolPart}
import org.icar.actor_model.protocol.GroundingProtocol
import org.icar.actor_model.protocol.GroundingProtocol.{AssignTask, CallForProposals, Proposal}

import scala.org.icar.high_level_specification.Task

case class ProposalRecord(proponent: ActorRef, msg:Proposal)

trait GroundingAuctioneer extends MUSARole {
	var worker_proposals : List[ProposalRecord] = List.empty

	def msg_grounding_call(task:Task): ProtocolPart = GroundingProtocol.init(task)

	def collect_proposals(task:Task): List[ProposalRecord] = {
		var new_work_prop : List[ProposalRecord] = List.empty
		var proposals : List[ProposalRecord] = List.empty
		for (p<-worker_proposals)
			if (p.msg.task.grounding.capability.id==task.grounding.capability.id)
				proposals = p :: proposals
			else
				new_work_prop = p :: new_work_prop
		worker_proposals = new_work_prop
		proposals
	}

	def decide_winner(prop:ProposalRecord) : Unit = {
		mylog("inform the winner")
		prop.proponent ! prop.msg.choose_as_winner
	}

	registerRole {
		case msg:Proposal =>
			mylog("proposal income")
			worker_proposals = ProposalRecord(sender,msg) :: worker_proposals

	}

}


trait GroundingAuctionParticipant extends MUSARole {

	def role__received_call_for_grounding_auction(sender: ActorRef, msg: CallForProposals): Unit
	def role__win_task_auction(sender: ActorRef, msg: AssignTask): Unit

	registerRole {
		case msg:CallForProposals =>
			mylog("auction call for"+msg.task.grounding.capability.id)
			role__received_call_for_grounding_auction(sender,msg)
		case msg:AssignTask =>
			mylog("wint the auction call for"+msg.task.grounding.capability.id)
			role__win_task_auction(sender,msg)
	}

}
