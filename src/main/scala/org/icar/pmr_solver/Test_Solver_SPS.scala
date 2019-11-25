package org.icar.pmr_solver

import org.icar.application.shipboard_power_system._
import org.icar.pmr_solver.high_level_specification._
import org.icar.pmr_solver.symbolic_level.HL2Raw_Map

object Test_Solver_SPS extends App {

/*
	val circuit = SPSCircuit.sample_circuit //prepare_circuit
	val mission = SPSCircuit.sample_circuit_mission
	val initial = SPSCircuit.sample_circuit_initial
*/

	val circuit = SPSCircuit.build_from_file("./data/sps_data/circuit3.txt") //prepare_circuit
	val mission = SPSCircuit.circuit_3_mission
	val initial = SPSCircuit.circuit_3_initial

	val types = circuit.generate_domain_types
	val preds = circuit.generate_predicates
	val axioms = circuit.generate_axioms
	val my_domain = Domain(preds,types,axioms)

	val map = new HL2Raw_Map(my_domain)
	val force_field = new ForceField(circuit,mission,map)


	val system_actions = circuit.generate_actions
	val env_actions : Array[EnvironmentAction] = Array.empty

	val goalmodel = LTLGoalSet(Array(
		circuit.generate_goal(mission)
	))

	val available = AvailableActions(system_actions,env_actions)
	val my_problem = Problem(initial,goalmodel,available)

	val solver = new Solver(my_problem,my_domain,force_field.qos)

	println("**Domain**")
	println("Number of predicates: "+solver.map.inverse.size)
	println("Number of goals: "+solver.specifications.length)
	println("Number of actions: "+solver.available_actions.length)
	println("Number of perturbations: "+solver.available_perturb.length)

	val its=solver.iterate_until_termination(
		SolverConfiguration(
			IterationTermination(20),//TimeTermination(100),
			SolutionConfiguration(
				allow_self_loop = false,
				allow_cap_multiple_instance = true,
				allow_loop = false,
				allow_parallel_action = true
			)
		)
	)

	if (solver.opt_solution_set.isDefined) {


		println("**Planning**")
		println("Number of iterations: "+its)

		println("**Solutions**")
		println("Number of generated WTS: "+solver.opt_solution_set.get.wts_list.size)
		println("Number of full WTS: "+solver.opt_solution_set.get.full_wts.size)
		println("Number of partial WTS: "+solver.opt_solution_set.get.partial_wts.size)

		val full = solver.opt_solution_set.get.full_wts.toList
		val sorted = full.sortBy(_.nodes.size)

		for (sol <- sorted)
			println( sol.to_graphviz( _.toString ) )
	}



}

