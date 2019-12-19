package org.icar.actor_model.core

import org.icar.pmr_solver.high_level_specification._
import org.icar.pmr_solver.symbolic_level.RawState

import scala.concurrent.duration.FiniteDuration
import scala.org.icar.high_level_specification.Solution
import scala.org.icar.pmr_solver.best_first_planner.SolutionConfiguration

case class ApplicationConfig(
	                            domain:Domain,
	                            availableAbstract : AvailableActions,
	                            monitors : Array[EnvObserver],
	                            monitor_delay : FiniteDuration,
	                            background_state : StateOfWorld,
	                            axioms : Array[Axiom],
	                            validator : Option[SolValidator],
	                            planner_heuristic : RawState => Float,
	                            planner_millisec: Long,
	                            planner_config : SolutionConfiguration,
	                            availableConcrete : Array[ConcreteCapability],
	                            grounding_delay : FiniteDuration,
															logfactory : MUSALoggerFactory,
                            )



trait EnvObserver {
	def variable_description : String
	def init_observer() : Unit
	def read_state : List[HL_GroundLiteral]
	def terminate_observer() : Unit
}

trait SolValidator {
	def validation_description : String
	def init_validator() : Unit
	def validate(sol:Solution) : MetaSolInfo
	def terminate_validator() : Unit
}

trait ConcreteCapability {
	def capability_description : String

	def ref_abstract : String
	def constraint: Map[String, ConstantTerm]

	def init_capability() : Unit
	def enter_in_team() : Unit
	def execute() : Unit
	def exit_from_team() : Unit
	def terminate_capability() : Unit
}


case class MetaSolInfo(valid: Boolean, qos: Float, sol:Solution)





