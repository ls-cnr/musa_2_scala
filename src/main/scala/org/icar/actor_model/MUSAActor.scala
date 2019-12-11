package org.icar.actor_model

import akka.actor.{Actor, ActorLogging}
import org.icar.pmr_solver.high_level_specification.{HL_GroundLiteral}

import scala.concurrent.ExecutionContextExecutor

trait MUSAActor extends Actor with ActorLogging {
	val system = context.system
	implicit val executionContext: ExecutionContextExecutor = system.dispatcher

}


trait EnvObserver {
	def variable_description : String
	def init_observer : Unit
	def read_state : List[HL_GroundLiteral]
	def terminate_observer : Unit
}
