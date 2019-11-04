package org.icar.pmr_solver

import org.icar.musa.context.StateOfWorld



// Luca: to implement:
// 1. R2S (when/where to use it?)
// 2. violation of temporal properties
//


class Solver(val problem: Problem,val domain: Domain) {

	var opt_solution_set : Option[SolutionSet] = None;
	val map = new PlanningVariableMap(domain)
	val I = RawState.factory(map.state_of_world(problem.I.statements.toList),domain.axioms)
	val goals = for (g<-problem.goal_model.goals) yield map.ltl_formula(g)
	val available_actions = for (a<-problem.actions.sys_action) yield map.system_action(a)
	val available_perturb = for (a<-problem.actions.env_action) yield map.environment_action(a)

	/* solver loop with termination conditions */
	def iterate_until_termination(conf : SolverConfiguration) : Int = {
		val start_timestamp: Long = System.currentTimeMillis

		opt_solution_set = Some( new SolutionSet(I, domain, goals,conf.sol_conf) )
		var n_iteration = 0
		var complete = false

		while (!complete && !check_termination(conf.termination,start_timestamp,n_iteration)) {
			iteration

			n_iteration += 1
		}

		n_iteration
	}

	/* Main algorithm of the Solver class */
	def iteration : Unit = {
		if (opt_solution_set.isDefined) {
			val solution_set = opt_solution_set.get
			val somenode = solution_set.get_next_node

			if (somenode.isDefined) {
				val node = somenode.get
				val actions = applicable_capabilities(node)
				val envs = applicable_perturbations(node)

				var exp_due_to_system : List[RawExpansion] = List.empty
				for (a <- actions) {
					exp_due_to_system = generate_system_expansion(node,a) :: exp_due_to_system
				}

				var exp_due_to_environment : List[RawExpansion] = List.empty
				for (e <- envs) {
					exp_due_to_environment = generate_environment_expansion(node,e) :: exp_due_to_environment
				}

				solution_set.update_the_wts_list(node,exp_due_to_system,exp_due_to_environment)
			}
		}
	}

	private def check_termination(term_condition : TerminationDescription, start : Long, it : Int) : Boolean = {
		term_condition match {
			case TimeTermination(time) =>
				val c_time = System.currentTimeMillis
				val delta = c_time-start
				delta >= time

			case IterationTermination(max_it) =>
				it >= max_it

			case AndTermination(l,r) =>
				check_termination(l,start,it) && check_termination(r,start,it)

			case OrTermination(l,r) =>
				check_termination(l,start,it) || check_termination(r,start,it)

			case _ =>
				false
		}
	}


	private def applicable_capabilities(node : RawState) : List[RawAction] = {
		var list : List[RawAction] = List.empty

		for (action <- available_actions) {
			val apply = node.satisfies(action.pre)
			if (apply)
				list = action :: list
		}

		list
	}

	private def applicable_perturbations(node : RawState) : List[RawAction] = {
		var list : List[RawAction] = List.empty

		for (action <- available_perturb) {
			val apply = node.satisfies(action.pre)
			if (apply)
				list = action :: list
		}

		list
	}

	private def generate_system_expansion(node : RawState, action : RawAction) : RawExpansion = {
		require(opt_solution_set.isDefined)

		val trajectory = for (effect <- action.effects) yield calculate_probabilistic_evolution(node,effect)
		RawExpansion(action,node,trajectory)
	}

	private def generate_environment_expansion(node : RawState, action : RawAction) : RawExpansion = {

		val trajectory: Array[ProbabilisticEvo] = for (effect <- action.effects) yield calculate_probabilistic_evolution(node,effect)
		RawExpansion(action,node,trajectory)
	}

	private def calculate_probabilistic_evolution(node : RawState, evo_description : RawEvolution) : ProbabilisticEvo = {
		require(opt_solution_set.isDefined)
		val evo_node = RawState.extend(node,evo_description)
		ProbabilisticEvo(evo_description.name,evo_description.probability,evo_node)
	}





}






