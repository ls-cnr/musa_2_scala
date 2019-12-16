package org.icar.actor_model

import akka.actor.{Actor, ActorLogging}
import org.icar.application.shipboard_power_system.{ForceField, SPSCircuit}
import org.icar.pmr_solver.high_level_specification._
import org.icar.pmr_solver.symbolic_level.{HL2Raw_Map, RawState}

import scala.concurrent.ExecutionContextExecutor
import scala.concurrent.duration.FiniteDuration
import scala.org.icar.high_level_specification.Solution
import scala.org.icar.pmr_solver.best_first_planner.SolutionConfiguration

trait MUSAActor extends Actor with ActorLogging {
	val system = context.system
	implicit val executionContext: ExecutionContextExecutor = system.dispatcher

	private var roles : List[Receive] = List(not_understood)
	protected def registerRole(receive: Receive) {
		roles = receive :: roles
	}

	private def not_understood : Receive = {
		case _ => NotUnderstood
	}

	def receive: Receive = roles reduce {_ orElse _}
}

trait MUSARole extends MUSAActor {
	registerRole(role_description)

	def role_description : Receive
}



case class NotUnderstood()

trait EnvObserver {
	def variable_description : String
	def init_observer : Unit
	def read_state : List[HL_GroundLiteral]
	def terminate_observer : Unit
}

trait SolValidator {
	def validation_description : String
	def init_validator : Unit
	def validate(sol:Solution) : MetaSolInfo
	def terminate_validator : Unit
}

trait ConcreteCapability {
	def capability_description : String
	def ref_abstract : String
	def init_capability : Unit
	def enter_in_team : Unit
	def execute : Unit
	def exit_from_team : Unit
	def terminate_capability : Unit
}


case class MetaSolInfo(valid: Boolean, qos: Float, sol:Solution)





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

                            )




object mySPSApp {
	import scala.concurrent.duration._

	// Medium Circuit
	val circuit = SPSCircuit.build_from_file("./data/sps_data/circuit3.txt") //prepare_circuit
	val mission = SPSCircuit.circuit_3_mission
	val initial = SPSCircuit.circuit_3_initial_totally_switched_off//circuit_3_initial_simple_failure

	val types = circuit.generate_domain_types
	val preds = circuit.generate_predicates
	val axioms = circuit.generate_axioms
	val my_domain = Domain("SPS",preds,types,axioms)

	val system_actions = circuit.generate_actions
	val env_actions : Array[AbstractCapability] = Array.empty
	val available = AvailableActions(system_actions,env_actions)

	val map = new HL2Raw_Map(my_domain)
	val force_field = new ForceField(circuit,mission,map)

	val sps = ApplicationConfig(
		domain = my_domain,
		availableAbstract = available,
		monitors = Array(),
		monitor_delay = 10 milliseconds,
		background_state = initial,
		axioms = Array(),
		validator = None,
		planner_heuristic = force_field.qos,
		planner_millisec = 1000,
		planner_config = SolutionConfiguration(
			allow_self_loop = false,
			allow_cap_multiple_instance = true,
			allow_loop = false,
			allow_parallel_action = true
		),
		availableConcrete = Array.empty,
		grounding_delay = 1 second,



	)


}
