package org.icar.application.shipboard_power_system

import org.icar.pmr_solver.high_level_specification.{AbstractCapability, AvailableActions, Domain, LTLGoalSet, Problem}
import org.icar.pmr_solver.nmc.{NMCSolver, WTSTreeNode}
import org.icar.pmr_solver.symbolic_level.HL2Raw_Map

import scala.org.icar.pmr_solver.best_first_planner.SolutionTermination

object Test_NMC_SPS extends App {

	val circuit = SPSCircuit.build_from_file("./data/sps_data/circuit3.txt") //prepare_circuit
	val mission = SPSCircuit.circuit_3_mission
	val initial = SPSCircuit.circuit_3_initial_totally_switched_off//circuit_3_initial_simple_failure


/*
	// Small Circuit
	val circuit = SPSCircuit.sample_circuit //prepare_circuit
	val mission = SPSCircuit.sample_circuit_mission
	val initial = SPSCircuit.sample_circuit_initial
*/

	val types = circuit.generate_domain_types
	val preds = circuit.generate_predicates
	val axioms = circuit.generate_axioms
	val my_domain = Domain("SPS",preds,types,axioms)

	val map = new HL2Raw_Map(my_domain)

	val system_actions = circuit.generate_actions
	val env_actions : Array[AbstractCapability] = Array.empty

	val goalmodel = LTLGoalSet(Array(
		circuit.generate_goal(mission)
	))

	val available = AvailableActions(system_actions,env_actions)
	val my_problem = Problem(initial,goalmodel,available)

	val solver = new NMCSolver(my_problem,my_domain)

	println("**Domain**")
	println("Number of predicates: "+solver.map.inverse.size)
	println("Number of goals: "+solver.specifications.length)
	println("Number of actions: "+solver.available_actions.length)

	//	val result = solver.nmcs(2,solver.tree.root)

	val it = solver.mcts_with_frontier(SolutionTermination(3))
	//val it = solver.mcts_with_frontier(TimeTermination(2000))

	println("**Planning**")
	println("Milliseconds: "+solver.elapsed)
	println("Number of iterations: "+it)
	println("Number of root iterations: "+solver.root_iterator)
	println("Number of visited nodes: "+WTSTreeNode.id)
	println("Number of nodes in frontier: "+solver.frontier.size)

	println("**Solutions**")
	println("Number of exit: "+solver.solutions)
	//println("Number of partial WTS: "+solver.opt_solution_set.get.partial_wts.size)

	solver.tree.update_wts_file("./data/sps_data/wts_tree.dot")
	println("terminated")
}
