package org.icar.actor_model

import org.icar.application.shipboard_power_system.SPSCircuit
import org.icar.pmr_solver.high_level_specification.{AbstractCapability, AvailableActions, Axiom, Domain, GroundPredicate}

import scala.concurrent.duration.FiniteDuration


case class ApplicationConfig(
	                            domain:Domain,
	                            available_actions : AvailableActions,
	                            monitors : Array[EnvObserver],
	                            monitor_delay : FiniteDuration,
	                            very_initial_state : List[GroundPredicate],
	                            axioms : Array[Axiom],




                            )



import scala.concurrent.duration._

object mySPSApp {
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



	val sps = ApplicationConfig(
		domain = my_domain,
		available_actions = available,
		monitors = Array(),
		monitor_delay = 10 milliseconds,
		very_initial_state = List.empty,
		axioms = Array(),

	)


}
