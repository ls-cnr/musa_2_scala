package org.icar.actor_model.protocol

import org.icar.actor_model.core.{Protocol, ProtocolPart}

import scala.org.icar.high_level_specification.{Solution, Task}

package object OrchestrationProtocol extends Protocol {

	trait ProcessCommitment {
		val id:Long
		val sol:Solution
		def not_enough_concrete : ProtocolPart = InformGroundingFailure(id,sol)
		def concrete_failure : ProtocolPart = InformConcreteFailure(id)
		def terminated : ProtocolPart = InformSolutionApplied(id,sol)
		def progress_task(task:Task) : ProtocolPart = InformProgress(id,task)
		def command_task(task:Task) : ProtocolPart = RequestConcreteExecution(id,task)
	}

	def init(sol:Solution) : ProtocolPart = RequestApplySolution(get_id,sol)
	def pause_orchestration(conv_id:Long) : ProtocolPart = PauseOrchestration(conv_id)
	def restart_orchestration(conv_id:Long) : ProtocolPart = RestartOrchestration(conv_id)
	def run_time_change(conv_id:Long, sol:Solution) : ProtocolPart = RequestSwitchSolution(conv_id,sol)

	case class RequestApplySolution private(id:Long, sol:Solution) extends ProtocolPart with ProcessCommitment
	case class RequestSwitchSolution private(id:Long, sol:Solution) extends ProtocolPart with ProcessCommitment

	case class RequestConcreteExecution(id:Long, task:Task) extends ProtocolPart
	case class PauseOrchestration private(id:Long) extends ProtocolPart
	case class RestartOrchestration private(id:Long) extends ProtocolPart

	case class InformProgress private(id:Long,task:Task) extends ProtocolPart
	case class InformGroundingFailure private(id:Long,sol:Solution) extends ProtocolPart
	case class InformSolutionApplied private(id:Long,sol:Solution) extends ProtocolPart

	case class InformConcreteFailure private(id:Long) extends ProtocolPart

}
