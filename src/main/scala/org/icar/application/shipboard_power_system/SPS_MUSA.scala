package org.icar.application.shipboard_power_system

import akka.actor.ActorSystem
import org.icar.actor_model.AdaptationMng
import org.icar.actor_model.core.{ApplicationConfig, ConsoleLoggerFactory, MultiTabLogger}
import org.icar.actor_model.protocol.InjectionProtocol
import org.icar.high_level_specification.AbstractCapability
import org.icar.pmr_solver.high_level_specification.{AvailableActions, Domain, LTLGoalSet}
import org.icar.pmr_solver.symbolic_level.HL2Raw_Map

import scala.org.icar.pmr_solver.best_first_planner.SolutionConfiguration

object SPS_MUSA extends App {
	// Medium Circuit
	val circuit = SPSCircuit.build_from_file("./data/sps_data/circuit3.txt") //prepare_circuit
	val mission = SPSCircuit.circuit_3_mission
	val initial = SPSCircuit.circuit_3_initial_totally_switched_off//circuit_3_initial_simple_failure

	val config = sps_app_config

	val system = ActorSystem("SPS_MUSA_System")
	val root_prop = AdaptationMng.instance(config)
	val root_actor = system.actorOf(root_prop,"AdaptManager")

	Thread.sleep(3000)
	val goal = circuit.generate_goal(mission)
	val goalmodel = LTLGoalSet(Array(
		goal
	))
	root_actor ! InjectionProtocol.init("single",goalmodel)





	private def sps_app_config : ApplicationConfig = {
		import scala.concurrent.duration._

		val types = circuit.generate_domain_types
		val preds = circuit.generate_predicates
		val axioms = circuit.generate_axioms
		val my_domain = Domain("SPS", preds, types, axioms)

		val system_actions = circuit.generate_actions
		val env_actions: Array[AbstractCapability] = Array.empty

		val available = AvailableActions(system_actions, env_actions)

		val map = new HL2Raw_Map(my_domain)
		val force_field = new ForceField(circuit, mission, map)

		ApplicationConfig(
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
			logfactory = new MultiTabLogger("SPS MUSA"), //new ConsoleLoggerFactory//

		)
	}


}
