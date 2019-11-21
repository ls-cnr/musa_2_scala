package org.icar.pmr_solver

import org.icar.application.shipboard_power_system._
import org.icar.pmr_solver.high_level_specification._
import org.icar.pmr_solver.symbolic_level.HL2Raw_Map

object Test_Solver_SPS extends App {

	def prepare_circuit : SPSCircuit = {
		val circuit = new SPSCircuit

		circuit.generators = List( Generator(1,100) )
		circuit.loads = List( Load(1,50),Load(2,50))
		circuit.nodes = List( SimpleNode(1),SimpleNode(2),SimpleNode(3),SimpleNode(4) )
		circuit.switchers = List(
			Switcher(1,SimpleNode(3),Load(1,50)),
			Switcher(2,SimpleNode(4),Load(2,50))
		)
		circuit.selectors = List(
			Selector(1,SimpleNode(1),Generator(1,100),SimpleNode(2)),
			Selector(1,SimpleNode(1),SimpleNode(3),SimpleNode(2)),
			Selector(2,SimpleNode(1),SimpleNode(4),SimpleNode(2))
		)

		circuit
	}

	val circuit = prepare_circuit
	val mission = SPSMission(List(1,2),List.empty,List.empty)


	val types = circuit.generate_domainn_types
	val preds = circuit.generate_predicates
	val axioms = circuit.generate_axioms
	val my_domain = Domain(preds,types,axioms)

	val map = new HL2Raw_Map(my_domain)
	val force_field = new ForceField(circuit,mission,map)

	val initial = StateOfWorld(List(
		GroundPredicate("on_gen",List(IntegerTerm(1))),

		GroundPredicate("pos1_sel",List(IntegerTerm(1))),
		GroundPredicate("pos1_sel",List(IntegerTerm(2))),
		GroundPredicate("pos1_sel",List(IntegerTerm(3)))
	))

	val system_actions = circuit.generate_actions
	val env_actions : Array[EnvironmentAction] = Array.empty

	val all_on = Conjunction[HL_LTLFormula](List(
		GroundPredicate("up_load", List(IntegerTerm(1))),
        GroundPredicate("up_load", List(IntegerTerm(2)))
	))
	val goalmodel = LTLGoalSet(Array(
		Finally(all_on)
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
			IterationTermination(5),//TimeTermination(100),
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

