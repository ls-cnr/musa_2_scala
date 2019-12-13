package org.icar.actor_model

import akka.actor.{ActorRef, Props}
import org.icar.WorkflowCase
import org.icar.actor_model.protocol.OrchestrationProtocol.ProcessCommitment
import org.icar.actor_model.protocol.{AdaptationProtocol, ContextProtocol, GroundingProtocol, OrchestrationProtocol}
import org.icar.pmr_solver.high_level_specification.HL_PredicateFormula
import org.icar.pmr_solver.symbolic_level.{HL2Raw_Map, RawPredicate, RawState}

import scala.org.icar.high_level_specification._

abstract class OrchestrationMng(config:ApplicationConfig) extends MUSAActor {
	val raw_map = new HL2Raw_Map(config.domain)

	var workers : List[ActorRef] = init_my_workers(config.availableConcrete)
	var orchestration_state = Self.init

	//todo - later, implement task proposal backlist


	case class ProposalRecord(proponent: ActorRef, abstract_id:String, concrete_id:String)


	object Self extends Protocol {
		def init : State = new Idle

		trait State {
			def applysolution_event(msg:ProcessCommitment) : State = this
			def switchsolution_event(msg:ProcessCommitment) : State = this
			def pause_request_event : State = this
			def restart_request_event : State = this
			def worker_complete_event(task:Task) : State = this
			def worker_failed_event(task:Task) : State = this
			def grounding_proposal(msg:ProposalRecord) : State = this
			def grounding_timeout : State = this
			def state_changes(state:RawState) : State = this
		}

		class Idle extends State {
			override def applysolution_event(msg:ProcessCommitment) : State = {
				new Grounding(msg)
			}
		}

		class Grounding(commitment:ProcessCommitment) extends State {
			case class AuctionTimeoutEvent()

			var proposals : List[ProposalRecord] = List.empty
			start_team_auction(commitment)
			set_grounding_timer

			override def grounding_proposal(msg:ProposalRecord) : State = {
				proposals = msg :: proposals
				this
			}
			override def grounding_timeout : State = {
				val team : Map[Task,ProposalRecord] = create_team(commitment,proposals)

				if (!team.isEmpty) {
					new Orchestrating(commitment,team)

				} else {
					sender ! commitment.not_enough_concrete
					new Idle
				}
			}

			/* utility functions */
			private def create_team(comm:ProcessCommitment,proposals : List[ProposalRecord]) : Map[Task,ProposalRecord] = {
				var attempt : Map[Task,ProposalRecord] = Map.empty
				var team_is_valid = true

				for (t<-comm.sol.wfitems if team_is_valid && t.isInstanceOf[Task]) {
					val task = t.asInstanceOf[Task]
					val opt_proposal = get_first_matching_proposal(task,proposals)
					if (opt_proposal.isDefined) {
						attempt += (task -> opt_proposal.get)
					} else {
						team_is_valid = false
					}
				}
				if (team_is_valid)
					attempt

				else
					Map.empty
			}
			private def get_first_matching_proposal(task: Task, proposals: List[ProposalRecord]) : Option[ProposalRecord] = {
				var selected : Option[ProposalRecord] = None

				for (p<-proposals if !selected.isDefined)
					if (p.abstract_id==task.grounding.c.id)
					//if p is not in black list
						selected = Some(p)

				selected
			}
			private def set_grounding_timer : Unit = {
				system.scheduler.scheduleOnce(config.grounding_delay, self, AuctionTimeoutEvent() )
			}

		}

		class Orchestrating(commitment:ProcessCommitment,team : Map[Task,ProposalRecord]) extends State {
			abstract class PoolItem
			case class SimpleItem(item:WorkflowItem) extends PoolItem
			case class TaskItem(pre:RawPredicate,task:Task) extends PoolItem
			case class MultiItem(decision:SplitGateway,succs:Array[Branch]) extends PoolItem
			case class Branch(scenario:String,pool_item:PoolItem)

			var current_state : Option[RawState] = None
			val wf_case = new WorkflowCase(config.domain,commitment.sol, ask_concrete_execution(commitment,team) )
			var orchestration_activity = false

			override def switchsolution_event(msg:ProcessCommitment) : State = this
			override def pause_request_event : State = {
				orchestration_activity = false
				// todo stop workers
				this
			}
			override def restart_request_event : State = {
				orchestration_activity = true
				// todo re-activate workers
				this
			}
			override def worker_complete_event(task:Task) : State = {
				wf_case.external_progress(task)
				wf_case.progress(current_state.get)
				this
			}
			override def worker_failed_event(task:Task) : State = {
				if (repair_local_failure)
					this
				else {
					inform_about_failure(commitment,task)
					new Idle
				}
			}
			override def state_changes(state:RawState) : State = {
				current_state = Some(state)
				wf_case.progress(state)
				this
			}

			/* private utility functions */
			private def repair_local_failure: Boolean = true

		}
	}


	final override def receive: Receive = {
		case msg@OrchestrationProtocol.RequestApplySolution(_,solution)	 =>
			orchestration_state.applysolution_event(msg)

		case msg@OrchestrationProtocol.RequestSwitchSolution(_,solution)	 =>
			orchestration_state.switchsolution_event(msg)

		case OrchestrationProtocol.PauseOrchestration(_)	 =>
			orchestration_state.pause_request_event

		case OrchestrationProtocol.RestartOrchestration(_)	 =>
			orchestration_state.restart_request_event

		case GroundingProtocol.Proposal(_,abstract_id,concrete_id)	 =>
			orchestration_state.grounding_proposal(ProposalRecord(sender,abstract_id,concrete_id))

		case GroundingProtocol.InformTaskProgress(_,_,_,_) =>

		case GroundingProtocol.InformCompletedTask(_,task,_) =>
			orchestration_state.worker_complete_event(task)

		case GroundingProtocol.InformFailedTask(_,task,_) =>
			orchestration_state.worker_failed_event(task)

		case msg:ContextProtocol.InformContextUpdate =>
			orchestration_state.state_changes(msg.current)

		case _=>
	}



	/* private communication functions */
	private def start_team_auction(comm:ProcessCommitment) : Unit = {
		for (t<-comm.sol.wfitems if t.isInstanceOf[Task]) {
			val task = t.asInstanceOf[Task]
			val call = GroundingProtocol.init(task.grounding.c.id)
			workers.foreach( _ ! call )
		}
	}
	private def ask_concrete_execution(commitment:ProcessCommitment,team : Map[Task,ProposalRecord])(t: Task):Unit =
		team(t).proponent ! commitment.command_task(t)
	private def inform_about_progress(commitment:ProcessCommitment,t: Task) : Unit =
		context.parent ! commitment.progress_task(t)
	private def inform_about_failure(commitment:ProcessCommitment,t: Task):Unit =
		context.parent ! AdaptationProtocol.concrete_failure(t.grounding.c.id)
	private def inform_solution_has_been_applied(commitment:ProcessCommitment):Unit =
		context.parent ! commitment.terminated







	def init_worker(concrete: ConcreteCapability) : ActorRef = {
		val props = WorkMng.instance(config,concrete)
		context.actorOf(props,concrete.capability_description+"_work")
	}

	def init_my_workers(cap_array:Array[ConcreteCapability]): List[ActorRef] = {
		(for (c<-cap_array) yield init_worker(c)) toList
	}

}


object OrchestrationMng {
	def instance(config:ApplicationConfig) : Props = Props.create(classOf[OrchestrationMng],config)


}
