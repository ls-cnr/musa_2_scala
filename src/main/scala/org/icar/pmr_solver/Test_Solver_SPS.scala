package org.icar.pmr_solver

import org.icar.application.shipboard_power_system.{ForceField, Generator, Load, SPSCircuit, SPSMission, SimpleNode, Switcher}
import org.icar.pmr_solver.high_level_specification.{AtomTerm, AvailableActions, Conjunction, Disjunction, Domain, DomainPredicate, DomainType, DomainVariable, EnvironmentAction, Finally, GroundPredicate, HL_LTLFormula, IntegerTerm, LTLGoalSet, NumericDomainType, Predicate, Problem, StateOfWorld, UnivQuantifier, VariableTerm}
import org.icar.pmr_solver.symbolic_level.{HL2Raw_Map, RawState}

object Test_Solver_SPS extends App {

	def prepare_sps_domain : Domain = {
		val dom_types : Array[DomainType] = Array(
			NumericDomainType("gen_id",1,1),
			NumericDomainType("load_id",1,2),
			NumericDomainType("node_id",1,3),
			NumericDomainType("sw_id",1,7),

		)

		val preds : Array[DomainPredicate] = Array(
			DomainPredicate("on_gen",List(
				DomainVariable("ID","gen_id")
			)),
			DomainPredicate("up_load",List(
				DomainVariable("ID","load_id")
			)),
			DomainPredicate("up_node",List(
				DomainVariable("ID","node_id")
			)),
			DomainPredicate("closed_sw",List(
				DomainVariable("ID","sw_id")
			))/*,
			DomainPredicate("off_gen",List(
				DomainVariable("ID","gen_id")
			)),
			DomainPredicate("down_load",List(
				DomainVariable("ID","load_id")
			)),
			DomainPredicate("down_node",List(
				DomainVariable("ID","node_id")
			)),
			DomainPredicate("open_sw",List(
				DomainVariable("ID","sw_id")
			))*/
		)

		Domain(preds,dom_types,Array.empty)
	}
	def prepare_circuit : SPSCircuit = {
		val circuit = new SPSCircuit

		circuit.generators = List( Generator(1,100) )
		circuit.loads = List( Load(1,50),Load(2,50))
		circuit.nodes = List( SimpleNode(1),SimpleNode(2),SimpleNode(3) )
		circuit.switcher = List(
			Switcher(1,Generator(1,100),SimpleNode(1)),
			Switcher(2,SimpleNode(1),SimpleNode(2)),
			Switcher(3,SimpleNode(1),SimpleNode(2)),
			Switcher(4,SimpleNode(1),SimpleNode(3)),
			Switcher(5,SimpleNode(1),SimpleNode(3)),
			Switcher(6,SimpleNode(2),Load(1,50)),
			Switcher(7,SimpleNode(3),Load(2,50))
		)

		circuit
	}

	val circuit = prepare_circuit
	val mission = SPSMission(List(1,2),List.empty,List.empty)

	val domain = prepare_sps_domain
	val map = new HL2Raw_Map(domain)

	val force_field = new ForceField(circuit,mission,map)

	val axioms = circuit.generate_axioms
	val my_domain = Domain(domain.predicates,domain.types,axioms)
/*
	val wi = RawState(map.state_of_world(List(
		GroundPredicate("on_gen",List(IntegerTerm(1))),

		GroundPredicate("closed_sw",List(IntegerTerm(1))),
		GroundPredicate("closed_sw",List(IntegerTerm(2))),
		GroundPredicate("closed_sw",List(IntegerTerm(4))),
		GroundPredicate("closed_sw",List(IntegerTerm(6)))
	)))
*/
	val initial = StateOfWorld(List(
		GroundPredicate("on_gen",List(IntegerTerm(1))),

		GroundPredicate("closed_sw",List(IntegerTerm(1))),
		GroundPredicate("closed_sw",List(IntegerTerm(2))),
		GroundPredicate("closed_sw",List(IntegerTerm(4))),
		GroundPredicate("closed_sw",List(IntegerTerm(6)))
	))

	val system_actions = circuit.generate_actions

	//val raw_action_clusters = for (a<-system_actions) yield map.system_action(a)
	//val raw_actions = raw_action_clusters.flatten

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
			IterationTermination(20),
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

		println( solver.opt_solution_set.get.all_solutions_to_graphviz(node => node.toString) )
	}



}

