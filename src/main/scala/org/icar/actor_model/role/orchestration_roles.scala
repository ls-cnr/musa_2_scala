package org.icar.actor_model.role

import akka.actor.ActorRef
import org.icar.actor_model.core.{MUSARole, ProtocolPart}
import org.icar.actor_model.protocol.OrchestrationProtocol
import org.icar.actor_model.protocol.OrchestrationProtocol._

import scala.org.icar.high_level_specification.{Solution, SolutionTask}

trait OrchestrationCustomerRole extends MUSARole {
	def msg_to_apply_solution(sol:Solution): ProtocolPart = OrchestrationProtocol.init(sol)
	def msg_to_switch_solution(msg:InformGroundingFailure,sol:Solution): ProtocolPart = msg.run_time_change(sol)

	def role__received_grounding_failure(sender: ActorRef, msg: InformGroundingFailure): Unit
	def role__received_workflow_completed(sender: ActorRef, msg: InformSolutionApplied): Unit

	registerRole {
		case msg:InformGroundingFailure =>
			mylog("workflow grounding failure")
			role__received_grounding_failure(sender,msg)
		case msg:InformSolutionApplied =>
			mylog("workflow completed")
			role__received_workflow_completed(sender,msg)
	}
}

trait OrchestrationDirectorRole extends MUSARole {
	var orchestration_request : Option[(ActorRef,ProcessCommitment)]=None
	var orchestration_status = false

	def start_orchestration : Unit = {
		orchestration_status = true
	}

	def msg_for_delegating_task_execution(task:SolutionTask): ProtocolPart = {
		require(orchestration_request.isDefined)
		orchestration_request.get._2.command_task(task)
	}

	def inform_about_grounding_failure(): Unit = {
		require(orchestration_request.isDefined)
		val customer = orchestration_request.get._1
		customer ! orchestration_request.get._2.not_enough_concrete
	}
	def inform_about_solution_applied(): Unit = {
		require(orchestration_request.isDefined)
		val customer = orchestration_request.get._1
		customer! orchestration_request.get._2.terminated
	}

	def role__received_request_for_orchestrating(sender: ActorRef, msg: ProcessCommitment): Unit
	def role__received_request_for_switch(sender: ActorRef, msg: ProcessCommitment): Unit
	def role__received_orchestration_pause(sender: ActorRef, msg: PauseOrchestration): Unit
	def role__received_orchestration_restart(sender: ActorRef, msg: RestartOrchestration): Unit
	def role__received_workflow_progress(sender: ActorRef, msg: InformProgress): Unit
	def role__received_concrete_failure(sender: ActorRef, msg: InformConcreteFailure): Unit

	registerRole {
		case msg:RequestApplySolution =>
			mylog("orchestration request")
			orchestration_request = Some(sender,msg)
			role__received_request_for_orchestrating(sender,msg)
		case msg:RequestSwitchSolution =>
			mylog("orchestration switch request")
			orchestration_request = Some(sender,msg)
			role__received_request_for_switch(sender,msg)
		case msg:PauseOrchestration =>
			mylog("orchestration pause")
			orchestration_status=false
			role__received_orchestration_pause(sender,msg)
		case msg:RestartOrchestration =>
			mylog("orchestration restart")
			orchestration_status=true
			role__received_orchestration_restart(sender,msg)
		case msg:InformProgress =>
			mylog("workflow progressing")
			role__received_workflow_progress(sender,msg)
		case msg:InformConcreteFailure =>
			mylog("concrete failure")
			role__received_concrete_failure(sender,msg)


	}
}

trait OrchestrationWorker extends MUSARole {
	def msg_to_request_solutions_for_problem(sol:Solution): ProtocolPart = OrchestrationProtocol.init(sol)

	def msg_to_inform_about_progress(request_msg: RequestConcreteExecution): Unit = {
		request_msg.progress_task
	}
	def msg_to_inform_about_concrete_failure(request_msg: RequestConcreteExecution): Unit = {
		request_msg.concrete_failure
	}

	def role__received_start_for_concrete(sender: ActorRef, msg: RequestConcreteExecution): Unit
	def role__received_concrete_pause(sender: ActorRef, msg: PauseConcrete): Unit
	def role__received_concrete_restart(sender: ActorRef, msg: RestartConcrete): Unit

	registerRole {
		case msg:RequestConcreteExecution =>
			mylog("start concrete execution")
			role__received_start_for_concrete(sender,msg)
		case msg:PauseConcrete =>
			mylog("concrete pause")
			role__received_concrete_pause(sender,msg)
		case msg:RestartConcrete =>
			mylog("concrete restart")
			role__received_concrete_restart(sender,msg)
	}
}
