package org.icar.pmr_solver

import net.sf.tweety.logics.fol.semantics.HerbrandInterpretation
import net.sf.tweety.logics.fol.syntax.FOLAtom
import net.sf.tweety.lp.asp.syntax.Program
import org.icar.fol.Entail.{head_for_asl, solver, tx}
import org.icar.musa.context.StateOfWorld



// Luca: general improvement: if two partial solutions terminates with the same state
// and their last couple of actions are the same with inverse order
// wI -A-> W1 -B-> W2
// wI -B-> W1 -A-> W2
// Then A and B can be parallelized:
// wI -A||B-> W2
// (see partial order reduction?)

//Remember: if wI -A-> W1 -B-> WI i.e. create a loop without exit, then NOT valid solution




/******* SOLUTIONS ********/

// Luca: isFullSolution - it is necessary to check for loop safety
// a loop is valid if there is the possibility to leave it and go towards a terminal state

class SolutionSet(val initial_w : StateOfWorld, domain : Domain, val goals : LTLGoalSet, conf : SolutionConfiguration) {

	private val base = new Program(domain.axioms_as_rulelist)
	val initial_state = state_checkin(initial_w)

	var wts_list : List[WTSGraph] = init()

	private def init() : List[WTSGraph] = {
		val supervisors = goals.getSupervisors(initial_state)
		val exit = LTLGoalSet.check_exit_node(supervisors)
		val frontier_set : Set[Node] = if (!exit) Set(initial_state) else Set.empty
		val terminal_set : Set[Node] = if (exit) Set(initial_state) else Set.empty
		val init_label = StateLabel(supervisors,exit,!exit,exit,exit,0)

		val labelling = WTSLabelling(
			frontier_set,
			terminal_set,
			Map(initial_state->init_label),
			0
		)

		List(WTSGraph(initial_state,Set(initial_state),Set.empty,Set.empty,labelling))
	}

	def full_wts : Array[WTSGraph] = {
		wts_list.filter( _.isFullSolution ).toArray
	}

	def partial_wts : Array[WTSGraph] = {
		wts_list.filter( _.isPartialSolution ).toArray
	}

	/* The node definition is common for all the WTS, thus it is created --once for all-- when a new state of world is generated */
	def state_checkin(w : StateOfWorld) : Node = {
		val w_base = base.clone()
		for (s <- w.statements)
			w_base.addFact(head_for_asl(s))

		val response = solver.computeModels(w_base, 10000)
		val interpretation = new HerbrandInterpretation()
		if (response != null) {
			val as = response.get(0)
			val it = as.iterator()

			while (it.hasNext) {
				val f = tx.toFOL(it.next())
				interpretation.add(f.asInstanceOf[FOLAtom])
			}
		}

		Node(w,interpretation)
	}


	/*
	 * returns the most promising node to be expanded.

	 * for all the WTS(s),
	 *  explore the wts frontier and
	 *  get the node with highest metric
	 */
	def get_next_node : Option[Node] = {
		var somenode : Option[Node] = None

		var node_value : Float = -1

		for (wts <- wts_list if !wts.wts_labelling.frontier.isEmpty)
			for (node_of_frontier <- wts.wts_labelling.frontier)
				if (wts.wts_labelling.labelling(node_of_frontier).metric > node_value) {
					somenode = Some(node_of_frontier)
					node_value = wts.wts_labelling.labelling(node_of_frontier).metric
				}

		somenode
	}


	/* given a focus node and a set of expansions, it updates all the corresponsing WTS where the exp(s) apply */
	def update_the_wts_list(focus : Node, exp_due_to_system: List[SystemExpansion], exp_due_to_environment: List[EnvironmentExpansion]): Unit = {
		var new_wts_list : List[WTSGraph] = List.empty

		/* check if the expansion is appliable to all the WTS that are not complete */
		for (wts : WTSGraph <- wts_list if !wts.wts_labelling.frontier.isEmpty)
			new_wts_list = WTSGraph.update_wts(wts,focus, exp_due_to_system,exp_due_to_environment,domain.qos,conf) ::: new_wts_list

		wts_list = new_wts_list //check_valid_paths(new_wts_list)
	}

/*
	private def check_valid_paths(wts_list:List[WTSGraph]) : List[WTSGraph] = {
		var allowed_wts_list : List[WTSGraph] = wts_list

		if (!conf.allow_loop)
			allowed_wts_list = remove_wts_with_loop(allowed_wts_list)
		else
			allowed_wts_list = remove_wts_with_non_valid_loop(allowed_wts_list)

		if (!conf.allow_cap_multiple_instance)
			allowed_wts_list = remove_wts_with_cap_multiple_instance(allowed_wts_list)

		if (conf.allow_parallel_action)
			allowed_wts_list = compress_paths_with_same_state(allowed_wts_list)

		allowed_wts_list
	}

	def remove_wts_with_loop(wts_list: List[WTSGraph]): List[WTSGraph] = wts_list.filter( _.do_not_containLoop )

	def remove_wts_with_non_valid_loop(wts_list: List[WTSGraph]): List[WTSGraph] = {
		var allowed_wts_list : List[WTSGraph] = wts_list.filter( _.do_not_contain_no_exit_loop )
		if (conf.allow_self_loop)
			allowed_wts_list = allowed_wts_list.filter( (_.do_not_containLoop) )

		allowed_wts_list
	}

	def remove_wts_with_cap_multiple_instance(wts_list: List[WTSGraph]): List[WTSGraph] =  wts_list.filter( _.do_not_contain_cap_multiple_instance )
*/


	def all_solutions_to_graphviz(pretty_string: Node => String) : String = {
		def node_label(n:Node, wts_counter: Int, pretty_string: Node => String) : String = {
			if (n==initial_state)
				pretty_string(initial_state)
			else
				wts_counter+"_"+pretty_string(n)
		}

		var string = "digraph WTS {\n"

		string += "\""+pretty_string(initial_state)+"\" [style=bold,color=yellow];\n"

		var wts_counter = 1

		for (wts <- wts_list) {
			for (n <- wts.nodes if n!=initial_state) {
				string += "\""+node_label(n,wts_counter,pretty_string)+"\""

				if (wts.wts_labelling.labelling(n).is_exit)
					string += "[style=bold,color=green];\n"
				else
					string += "[color=black];\n"
			}


			for (t <- wts.transitions) {
				string += "\""+node_label(t.origin,wts_counter,pretty_string)+"\""
				string += "->"
				string += "\""+node_label(t.destination,wts_counter,pretty_string)+"\""
				string += "[label=\""+t.action.id+"_"+t.scenario_name+"\"];\n"
			}

			for (t <- wts.perturbations) {
				string += "\""+node_label(t.origin,wts_counter,pretty_string)+"\""
				string += "->"
				string += "\""+node_label(t.destination,wts_counter,pretty_string)+"\""
				string += "[style=dotted, label=\""+t.env_action.id+"_"+t.probability+"% \"];\n"
			}

			wts_counter += 1
		}

		string += "}\n"

		string
	}



}


