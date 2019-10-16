package org.icar.pmr_solver

import net.sf.tweety.logics.fol.semantics.HerbrandInterpretation
import net.sf.tweety.logics.fol.syntax.FOLAtom
import net.sf.tweety.lp.asp.syntax.Program
import org.icar.fol.Entail.{head_for_asl, solver, tx}
import org.icar.musa.context.StateOfWorld



/******* SOLUTIONS ********/

// Luca: isFullSolution - it is necessary to check for loop safety
// a loop is valid if there is the possibility to leave it and go towards a terminal state

class SolutionSet(val initial_w : StateOfWorld, domain : Domain, val goals : LTLGoalSet) {

	private val base = new Program(domain.axioms_as_rulelist)
	val initial_state = state_checkin(initial_w)

	var wts_list : List[ExpWTS] = init()

	private def init() : List[ExpWTS] = {
		val supervisors = goals.getSupervisors(initial_state)
		val mapping = StateLabel(initial_state,supervisors,0)
		List(ExpWTS(Set(initial_state),Set.empty,Set.empty,List(mapping),List.empty,0))
	}

	def full_wts : Array[ExpWTS] = {
		wts_list.filter( isFullSolution(_) ).toArray
	}

	def partial_wts : Array[ExpWTS] = {
		wts_list.filter( isPartialSolution(_) ).toArray
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

		for (wts <- wts_list if !wts.frontier.isEmpty)
			for (f <- wts.frontier)
				if (f.metric > node_value) {
					somenode = Some(f.state)
					node_value = f.metric
				}

		somenode
	}


	/* given a focus node and a set of expansions, it updates all the corresponsing WTS where the exp(s) apply */
	def update_the_wts_list(focus : Node, exp_due_to_system: List[SystemExpansion], exp_due_to_environment: List[EnvironmentExpansion]): Unit = {
		var new_wts_list : List[ExpWTS] = List.empty

		/* check if the expansion is appliable to all the WTS that are not complete */
		for (wts : ExpWTS <- wts_list if !wts.frontier.isEmpty)
			new_wts_list = split_wts(wts,focus, exp_due_to_system,exp_due_to_environment) ::: new_wts_list

		wts_list = new_wts_list
	}


	/*
	 * This function is the core of WTS expansion:
	 * expanding a single WTS often means split it into a number of alternative possibilities
	 */
	def split_wts(wts : ExpWTS, focus_node : Node, exp_due_to_system: List[SystemExpansion], exp_due_to_environment: List[EnvironmentExpansion]) : List[ExpWTS] = {
		var updated_list : List[ExpWTS] = List.empty

		/* IF this WTS is involved into the system expansion */
		if (wts.nodes.contains(focus_node)) {

			/* THEN split the WTS for applying all the expansions due to system actions */
			for (exp <- exp_due_to_system) {
				var new_nodes : Set[Node] = Set.empty
				var new_transition : Set[TransitionArc] = Set.empty
				var new_perturb : Set[PerturbationArc] = Set.empty

				for (evolution_part <- exp.trajectory) {
					val evolution_node = evolution_part.dest

					if (!wts.nodes.contains(evolution_part.dest)) {
						new_nodes = new_nodes + evolution_part.dest
					}


					new_transition = new_transition + TransitionArc(exp.from, evolution_part.dest, exp.due_to, evolution_part.name)
				}

				/* AND check for applying all the expansions due to environment actions */
				for (pert <- exp_due_to_environment) {
					for (perturb_part <- pert.probtrajectory) {
						if (!wts.nodes.contains(perturb_part.dest))
							new_nodes = new_nodes + perturb_part.dest

						new_perturb = new_perturb + PerturbationArc(pert.from, perturb_part.dest, perturb_part.probability, pert.due_to, perturb_part.name)
					}
				}

				/* update the frontier and the quality, PS: a WTS is complete when the frontier is empty */
				val updated_frontier_leaf : (List[StateLabel],List[StateLabel]) = update_mapping(wts,focus_node,new_nodes,new_transition,new_perturb)
				val updated_frontier = updated_frontier_leaf._1
				val updated_leaf = updated_frontier_leaf._2
				val quality = calculate_quality_of_solution(wts,focus_node,updated_frontier,new_nodes,new_transition,new_perturb)

				/* FINALLY, the new list of WTS will contain the cloned updated WTS */
				updated_list = ExpWTS(wts.nodes++new_nodes,wts.transitions++new_transition,wts.perturbations++new_perturb,updated_frontier,updated_leaf,quality) :: updated_list
			}


			/* OTERWISE preserve the WTS into the new list of WTS */
		} else {

			updated_list = wts :: updated_list

		}

		/* RETURN the list of updated WTS (spitted and originals) */
		updated_list
	}



	/*
	 * This function calculates the frontier of the new WTS
	 *
	 * The new frontier is computed by removing the focus_node and
	 * adding all the new generated node, updating goals and qos
	*/
	def update_mapping(old_wts: ExpWTS, focus_node: Node, new_nodes: Set[Node], new_transition: Set[TransitionArc], new_perturb: Set[PerturbationArc]) : (List[StateLabel],List[StateLabel]) = {
		var focus_label : StateLabel = old_wts.frontier.find( _.state == focus_node).get

		/* remove focus node from frontier */
		var new_frontier : List[StateLabel] = old_wts.frontier.filter(_ != focus_label)

		/* all the previous leaf are again leaf in this new WTS */
		var new_leaf : List[StateLabel] = old_wts.terminal

		/* add new nodes to the frontier */
		for (node <- new_nodes) {
			val updated_array : Array[GoalSupervisor] = for (l <- focus_label.sup_array) yield l.getNextSupervisor(node)
			val updated_metric : Float = domain.qos(node)

			/* check if this new node is an exit node */
			val exit_node = check_exit_node(updated_array)

			/* put all the new nodes, except for exit ones, into the frontier */
			if (!exit_node)
				new_frontier = StateLabel(node,updated_array,updated_metric) :: new_frontier
			else
				new_leaf = StateLabel(node,updated_array,updated_metric) :: new_leaf
		}

		(new_frontier,new_leaf)
	}


	/*
	 * This function calculates the quality of the new WTS
	 *
	 * PROVVISORIAMENTE: quality = average of frontier node values
	 */
	def calculate_quality_of_solution(old_wts: ExpWTS, focus_node: Node, updated_frontier : List[StateLabel], new_nodes: Set[Node], new_transition: Set[TransitionArc], new_perturb: Set[PerturbationArc]): Float = {
		var q : Float = 0
		for (f <- updated_frontier)
			q+=f.metric

		q/updated_frontier.size
	}


	/*
	 * This function checks if a particular node fully satisfies the set of goals
	 *
	 */
	def check_exit_node(sups : Array[GoalSupervisor]) : Boolean = {
		var exit=true
		for (s <- sups)
			if (!s.isFullSatisfied)
				exit = false

		exit
	}

	def isFullSolution(wts:ExpWTS) : Boolean = {
		var full=true

		for (s <- wts.terminal) {
			if (!check_exit_node(s.sup_array))
				full=false
		}

		full
	}
	def isPartialSolution(wts:ExpWTS) : Boolean = !isFullSolution(wts)


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

				if (wts.node_is_terminal(n))
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


/******* WTS ********/

case class ExpWTS(
	                 nodes : Set[Node],
	                 transitions : Set[TransitionArc],
	                 perturbations : Set[PerturbationArc],
	                 frontier : List[StateLabel],
	                 terminal : List[StateLabel],
	                 quality_of_solution : Float
                 ) {

	def node_is_terminal(node: Node) : Boolean = {
		var is_leaf=false
		for (l <- terminal)
			if (l.state==node)
				is_leaf=true
		is_leaf
	}
}

case class Node(w : StateOfWorld, interpretation : HerbrandInterpretation)
case class TransitionArc(origin : Node, destination : Node, action: SystemAction, scenario_name : String)
case class PerturbationArc(origin : Node, destination : Node, probability : Float, env_action: EnvironmentAction, scenario_name : String)

case class StateLabel(state : Node, sup_array : Array[GoalSupervisor], metric : Float)


/******* STATE EVOLUTIONS ********/

class Expansion
case class SystemExpansion(due_to : SystemAction, from : Node, trajectory : Array[Evo])
case class EnvironmentExpansion(due_to : EnvironmentAction, from : Node, probtrajectory : Array[ProbabilisticEvo])
case class Evo (name: String, dest : Node)
case class ProbabilisticEvo (name: String, probability : Float, dest : Node)

