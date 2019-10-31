package org.icar.pmr_solver

import org.icar.musa.context.StateOfWorld



// Luca: to implement:
// 1. R2S (when/where to use it?)
// 2. violation of temporal properties
//


class Solver(val problem: Problem,val domain: Domain) {

	var opt_solution_set : Option[SolutionSet] = None;


	/* solver loop with termination conditions */
	def iterate_until_termination(conf : SolverConfiguration) : Int = {
		val start_timestamp: Long = System.currentTimeMillis

		opt_solution_set = Some( new SolutionSet(problem.I, domain, problem.goal_model,conf.sol_conf) )
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

				var exp_due_to_system : List[SystemExpansion] = List.empty
				for (a <- actions) {
					exp_due_to_system = generate_system_expansion(node,a) :: exp_due_to_system
				}

				var exp_due_to_environment : List[EnvironmentExpansion] = List.empty
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


	private def applicable_capabilities(node : Node) : List[SystemAction] = {
		var list : List[SystemAction] = List.empty

		for (action <- problem.actions.sys_action) {
			val apply = node.interpretation.satisfies(action.pre)
			if (apply)
				list = action :: list
		}

		list
	}

	private def applicable_perturbations(node : Node) : List[EnvironmentAction] = {
		var list : List[EnvironmentAction] = List.empty

		for (action <- problem.actions.env_action) {
			val apply = node.interpretation.satisfies(action.pre)
			if (apply)
				list = action :: list
		}

		list
	}

	private def generate_system_expansion(node : Node, action : SystemAction) : SystemExpansion = {
		require(opt_solution_set.isDefined)

		def calculate_evolution(node : Node, evo_description : EvolutionGrounding) : Evo = {
			val w2 = StateOfWorld.extend(node.w, evo_description.evo)
			val n2 = opt_solution_set.get.state_checkin(w2)
			Evo(evo_description.name,n2)
		}

		val trajectory: Array[Evo] = for (effect <- action.effects) yield calculate_evolution(node,effect)
		SystemExpansion(action,node,trajectory)
	}

	private def generate_environment_expansion(node : Node, action : EnvironmentAction) : EnvironmentExpansion = {

		def calculate_probabiliostic_evolution(node : Node, evo_description : ProbabilisticEvolutionGrounding) : ProbabilisticEvo = {
			require(opt_solution_set.isDefined)

			val w2 = StateOfWorld.extend(node.w, evo_description.evo)
			val n2 = opt_solution_set.get.state_checkin(w2)
			ProbabilisticEvo(evo_description.name,evo_description.probability,n2)
		}

		val trajectory: Array[ProbabilisticEvo] = for (effect <- action.effects) yield calculate_probabiliostic_evolution(node,effect)
		EnvironmentExpansion(action,node,trajectory)
	}






}






