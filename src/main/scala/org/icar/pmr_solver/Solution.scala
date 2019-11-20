package org.icar.pmr_solver

import org.icar.pmr_solver.rete.{RETE, RETEMemory}
import org.icar.pmr_solver.symbolic_level.{RawExpansion, RawGoalModelSupervisor, RawState}


/******* NOTES AND COMMENTS ********/
// Luca: general improvement: if two partial solutions terminates with the same state
// and their last couple of actions are the same with inverse order
// wI -A-> W1 -B-> W2
// wI -B-> W1 -A-> W2
// Then A and B can be parallelized:
// wI -A||B-> W2
// (see partial order reduction?)

//Remember: if wI -A-> W1 -B-> WI i.e. create a loop without exit, then NOT valid solution




/******* SOLUTIONS ********/

class SolutionSet(val rete_memory : RETEMemory, qos : RawState => Float, val init_goal_sup : RawGoalModelSupervisor, conf : SolutionConfiguration) {
	val initial_state = rete_memory.current
	var wts_list : List[WTSGraph] = init()

	private def init() : List[WTSGraph] = {
		val exit = init_goal_sup.check_exit_node
		val frontier_set : Set[RETEMemory] = if (!exit) Set(rete_memory) else Set.empty
		val terminal_set : Set[RawState] = if (exit) Set(initial_state) else Set.empty
		val init_label = StateLabel(init_goal_sup,exit,!exit,exit,exit,0)

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
			for (node_of_frontier <- wts.wts_labelling.frontier) {
				val state = node_of_frontier.current
				val label = wts.wts_labelling.nodes_labelling(state)
				if (label.metric > node_value) {
					somenode = Some( Node(node_of_frontier, label.sup_array ))
					node_value = label.metric
				}
			}

		somenode
	}


	/* given a focus node and a set of expansions, it updates all the corresponsing WTS where the exp(s) apply */
	def update_the_wts_list(focus : RawState, exp_due_to_system: List[RawExpansion], exp_due_to_environment: List[RawExpansion]): Unit = {
		var new_wts_list : List[WTSGraph] = List.empty

		/* check if the expansion is appliable to all the WTS that are not complete */
		for (wts : WTSGraph <- wts_list)
			if (wts.wts_labelling.frontier.isEmpty)
				new_wts_list = wts :: new_wts_list
			else
				new_wts_list = WTSGraph.update_wts(wts,focus, exp_due_to_system,exp_due_to_environment,qos,conf) ::: new_wts_list

		wts_list = new_wts_list //check_valid_paths(new_wts_list)
	}

	def all_solutions_to_graphviz(pretty_string: RawState => String) : String = {
		def node_label(n:RawState, wts_counter: Int, pretty_string: RawState => String) : String = {
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

				if (wts.wts_labelling.nodes_labelling(n).is_exit)
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
				string += "[style=dotted, label=\""+t.action.id+"_"+t.probability+"% \"];\n"
			}

			wts_counter += 1
		}

		string += "}\n"

		string
	}

}


