package org.icar.actor_model

import akka.actor.{ActorRef, Props}
import org.icar.WorkflowCase
import org.icar.actor_model.core.{ApplicationConfig, ConcreteCapability, MUSAActor, Protocol}
import org.icar.actor_model.protocol.OrchestrationProtocol.ProcessCommitment
import org.icar.actor_model.protocol.{ContextProtocol, OrchestrationProtocol}
import org.icar.actor_model.role.{ContextConsumerRole, GroundingAuctioneer, OrchestrationDirector, ProposalRecord}
import org.icar.pmr_solver.symbolic_level.{HL2Raw_Map, RawState}

import scala.org.icar.high_level_specification._

class OrchestrationMng(config:ApplicationConfig) extends MUSAActor
	with ContextConsumerRole
	with OrchestrationDirector
	with GroundingAuctioneer {
	registerRole(Self.internal_role)

	val my_log_area = config.logfactory.register_actor(self.path.name)
	mylog("welcome to OrchestrationMng !")

	val raw_map = new HL2Raw_Map(config.domain)

	var current_state : Option[RawState] = None
	var workers : List[ActorRef] = init_my_workers(config.availableConcrete)
	var orchestration_state = Self.init

	//todo - later, implement task proposal backlist

	override def preStart(): Unit = {

		context.parent ! register_to_context
	}

	object Self extends Protocol {
		def init : State = new Idle

		case class AuctionTimeoutEvent()
		def internal_role : Receive = {
			case AuctionTimeoutEvent() =>
				orchestration_state = orchestration_state.grounding_timeout
		}

		trait State {
			def applysolution_event(msg:ProcessCommitment) : State = this
			def switchsolution_event(msg:ProcessCommitment) : State = this
			def pause_request_event : State = this
			def restart_request_event : State = this
			def worker_complete_event(task:Task) : State = this
			def worker_failed_event(task:Task) : State = this
			def grounding_timeout : State = this
			def state_changes(state:RawState) : State = {
				current_state = Some(state)
				this
			}
		}

		class Idle extends State {
			mylog("orchestrator_mng-state=Idle")
			override def applysolution_event(msg:ProcessCommitment) : State = {
				new Grounding(msg)
			}
		}

		class Grounding(commitment:ProcessCommitment) extends State {
			mylog("orchestrator_mng-state=Grounding")
			start_team_auction(commitment)
			set_grounding_timer

			override def grounding_timeout : State = {
				mylog("auction-timeout")
				val team : Map[Task,ProposalRecord] = create_team(commitment)

				if (!team.isEmpty) {
					new Orchestrating(commitment,team)

				} else {
					inform_about_grounding_failure()
					new Idle
				}
			}

			/* utility functions */
			private def start_team_auction(comm:ProcessCommitment) : Unit = {
				mylog("start-auction")
				for (t<-comm.sol.wfitems if t.isInstanceOf[Task]) {
					val task = t.asInstanceOf[Task]
					val call = msg_grounding_call(task)
					workers.foreach( _ ! call )
				}
			}
			private def set_grounding_timer : Unit = {
				mylog("start-timeout")
				system.scheduler.scheduleOnce(config.grounding_delay, self, AuctionTimeoutEvent() )
			}
			private def create_team(comm:ProcessCommitment) : Map[Task,ProposalRecord] = {
				var attempt : Map[Task,ProposalRecord] = Map.empty
				var team_is_valid = true

				for (t<-comm.sol.wfitems if team_is_valid && t.isInstanceOf[Task]) {
					val task = t.asInstanceOf[Task]

					val proposals = collect_proposals(task)
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
					if (p.msg.task.grounding.capability.id==task.grounding.capability.id)
						//if p is not in black list
						selected = Some(p)

				selected
			}

		}

		class Orchestrating(commitment:ProcessCommitment,team : Map[Task,ProposalRecord]) extends State {
			mylog("orchestrator_mng-state=Orchestrating")
			val wf_case = new WorkflowCase(config.domain,commitment.sol, ask_concrete_execution(commitment,team) )
			var orchestration_activity = false

			override def switchsolution_event(msg:ProcessCommitment) : State = {
				// todo strategy for wf switch
				this
			}
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
				if (wf_case.case_pool.nonEmpty) {
					wf_case.external_progress(task)
					wf_case.progress(current_state.get)
				} else {
					inform_solution_has_been_applied
				}
				this
			}
			override def worker_failed_event(task:Task) : State = {
				if (repair_local_failure)
					this
				else {
					inform_about_failure
					new Idle
				}
			}
			override def state_changes(state:RawState) : State = {
				current_state = Some(state)
				wf_case.progress(state)
				this
			}

			/* private utility functions */
			private def ask_concrete_execution(commitment:ProcessCommitment,team : Map[Task,ProposalRecord])(t: Task):Unit ={
				team(t).proponent ! msg_for_delegating_task_execution(t)
			}
			private def repair_local_failure: Boolean = true
			private def inform_about_failure:Unit ={
				inform_about_grounding_failure()
			}
			private def inform_solution_has_been_applied:Unit =
				inform_about_solution_applied()

		}
	}

	override def role__received_context_update(sender: ActorRef, msg: ContextProtocol.InformContextUpdate): Unit = {
		orchestration_state = orchestration_state.state_changes(msg.current)
	}
	override def role__received_goal_violation(sender: ActorRef, msg: ContextProtocol.InformGoalViolation): Unit = {}
	override def role__received_request_for_orchestrating(sender: ActorRef, msg: ProcessCommitment): Unit = {
		orchestration_state = orchestration_state.applysolution_event(msg)
	}
	override def role__received_request_for_switch(sender: ActorRef, msg: ProcessCommitment): Unit = {
		orchestration_state = orchestration_state.switchsolution_event(msg)
	}
	override def role__received_orchestration_pause(sender: ActorRef, msg: OrchestrationProtocol.PauseOrchestration): Unit = {
		orchestration_state = orchestration_state.pause_request_event
	}
	override def role__received_orchestration_restart(sender: ActorRef, msg: OrchestrationProtocol.RestartOrchestration): Unit = {
		orchestration_state = orchestration_state.restart_request_event
	}
	override def role__received_workflow_progress(sender: ActorRef, msg: OrchestrationProtocol.InformProgress): Unit = {
		orchestration_state = orchestration_state.worker_complete_event(msg.task)
	}
	override def role__received_concrete_failure(sender: ActorRef, msg: OrchestrationProtocol.InformConcreteFailure): Unit = {
		orchestration_state = orchestration_state.worker_failed_event(msg.task)
	}


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
