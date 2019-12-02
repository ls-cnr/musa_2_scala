package org.icar.application.shipboard_power_system

import org.icar.application.shipboard_power_system.Test_NMC_SPS.solver
import org.icar.pmr_solver.high_level_specification._
import org.icar.pmr_solver.nmc.R2S
import org.icar.pmr_solver.symbolic_level.{HL2Raw_Map, RawGoalModelSupervisor, RawLTL}
import org.icar.pmr_solver.{IterationTermination, SolutionConfiguration, SolutionTermination, Solver, SolverConfiguration, TimeTermination}

object Test_Solver_SPS extends App {
	/*

		// Small Circuit
		val circuit = SPSCircuit.sample_circuit //prepare_circuit
		val mission = SPSCircuit.sample_circuit_mission
		val initial = SPSCircuit.sample_circuit_initial
	*/

	// Medium Circuit
	val circuit = SPSCircuit.build_from_file("./data/sps_data/circuit3.txt") //prepare_circuit
	val mission = SPSCircuit.circuit_3_mission
	val initial = SPSCircuit.circuit_3_initial_totally_switched_off//circuit_3_initial_simple_failure

	val types = circuit.generate_domain_types
	val preds = circuit.generate_predicates
	val axioms = circuit.generate_axioms
	val my_domain = Domain(preds,types,axioms)

	val map = new HL2Raw_Map(my_domain)
	val force_field = new ForceField(circuit,mission,map)

	val system_actions = circuit.generate_actions
	val env_actions : Array[AbstractCapability] = Array.empty

	val goal = circuit.generate_goal(mission)
	val goalmodel = LTLGoalSet(Array(
		goal
	))

	val available = AvailableActions(system_actions,env_actions)
	val my_problem = Problem(initial,goalmodel,available)

	//val solver = new Solver(my_problem,my_domain,_=>0)
	val specifications = map.ltl_formula(goal)
	val solver = new Solver(my_problem,my_domain,R2S.metric(specifications))
	//val solver = new Solver(my_problem,my_domain,force_field.qos)

	println("**Domain**")
	println("Number of predicates: "+solver.map.inverse.size)
	println("Number of goals: "+solver.specifications.length)
	println("Number of actions: "+solver.available_actions.length)
	println("Number of perturbations: "+solver.available_perturb.length)

	val its=solver.iterate_until_termination(
		SolverConfiguration(
			SolutionTermination(3),
			//TimeTermination(2000),//IterationTermination(20),//TimeTermination(100),
			SolutionConfiguration(
				allow_self_loop = false,
				allow_cap_multiple_instance = true,
				allow_loop = false,
				allow_parallel_action = true
			)
		)
	)

	println("**Planning**")
	println("Milliseconds: "+solver.elapsed)
	println("Number of iterations: "+its)
	println("Number of nodes: "+solver.num_nodes)

	if (solver.opt_solution_set.isDefined) {
		println("**Solutions**")
		println("Number of generated WTS: "+solver.opt_solution_set.get.wts_list.size)
		println("Number of full WTS: "+solver.opt_solution_set.get.full_wts.size)
		println("Number of partial WTS: "+solver.opt_solution_set.get.partial_wts.size)

		val full = solver.opt_solution_set.get.full_wts.toList
		val sorted = full.sortBy(_.nodes.size)

		var sol_num=0
		for (sol <- sorted){
			if (sol_num==0)
				sol.update_wts_file(circuit.pretty_string(map), s"./data/sps_data/wts_sol$sol_num.dot")
			sol_num += 1
		}
	}

}
