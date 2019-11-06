package org.icar.pmr_solver

import net.sf.tweety.logics.fol.semantics.HerbrandInterpretation
import org.icar.fol.{FOLCondition, GroundLiteral, GroundPredicate, TweetyFormula}
import org.icar.musa.context.StateOfWorld

/******* WTS GRAPH ********/
// Luca: un WTS contiene un -labelling- una mappa di tutti gli StateLabel del grafo

case class WTSGraph(
	                 start : RawState,
	                 nodes : Set[RawState],
	                 transitions : Set[RawArc],
	                 perturbations : Set[RawArc],

	                 wts_labelling : WTSLabelling

                 ) {

	def node_is_terminal(node: RawState) : Boolean = wts_labelling.terminal.contains(node)

	def isFullSolution : Boolean = {
		var full=true

		for (terminal_node <- wts_labelling.terminal++wts_labelling.frontier) {
			val sup_array = wts_labelling.labelling(terminal_node).sup_array
			if (!LTLGoalSet.check_exit_node(sup_array))
				full=false
		}

		full
	}
	def isPartialSolution : Boolean = !isFullSolution

/*	def do_not_containLoop : Boolean = wts_labelling.loop_transitions.isEmpty

	def do_not_contain_selfLoop : Boolean = {
		var resp = true
		if (!wts_labelling.loop_transitions.isEmpty){
			for (t <- wts_labelling.loop_transitions)
				if (t.origin==t.destination)
					resp = false
		}
		resp
	}

	def do_not_contain_no_exit_loop : Boolean = ???

	def do_not_contain_cap_multiple_instance : Boolean = ???
*/
	def to_graphviz(pretty_string: RawState => String) : String = {

		var string = "digraph WTS {\n"

		for (n <- nodes) {
			string += "\""+pretty_string(n)+"\""

			if (wts_labelling.labelling(n).is_exit)
				string += "[style=bold,color=green];\n"
			else
				string += "[color=black];\n"
		}


		for (t <- transitions) {
			string += "\""+pretty_string(t.origin)+"\""
			string += "->"
			string += "\""+pretty_string(t.destination)+"\""
			string += "[label=\""+t.action.id+"_"+t.scenario_name+"\"];\n"
		}

		for (t <- perturbations) {
			string += "\""+pretty_string(t.origin)+"\""
			string += "->"
			string += "\""+pretty_string(t.destination)+"\""
			string += "[style=dotted, label=\""+t.action.id+"_"+t.probability+"% \"];\n"
		}

		string += "}\n"

		string
	}

}

object WTSGraph {

	def check_pre_expansion_validity_test(wts: WTSGraph, exp: RawExpansion, conf: SolutionConfiguration):Boolean = {
		var multiple_cap_test = true
		if(!conf.allow_cap_multiple_instance) {
			for (t<-wts.transitions)
				if (t.action.id == exp.due_to.id)
					multiple_cap_test=false
		}
		multiple_cap_test
	}

	def check_post_expansion_validity_test(wts: WTSGraph, new_nodes: Set[RawState], new_transitions: Set[RawArc], conf: SolutionConfiguration):Boolean = {
		var self_loop_test = true
		if (!conf.allow_self_loop)
			for (t<-new_transitions)
				if (t.origin == t.destination)
					self_loop_test=false

		var loop_test = true
		if (!conf.allow_loop)
			for (t<-new_transitions)
				if (wts.nodes.contains(t.destination))
					loop_test=false

		self_loop_test && loop_test
	}

	def update_wts(wts:WTSGraph, focus : RawState, exp_due_to_system: List[RawExpansion], exp_due_to_environment: List[RawExpansion], qos : RawState => Float, conf : SolutionConfiguration) : List[WTSGraph] = {

		if (!wts.nodes.contains(focus)) {

			List(wts)

		} else if (!wts.wts_labelling.labelling(focus).is_exit && exp_due_to_system.nonEmpty) {

			var updated_list : List[WTSGraph] = List.empty

			for (exp <- exp_due_to_system) {

				val pre_test = check_pre_expansion_validity_test(wts,exp,conf)
				if (pre_test) {
					val sys_res : (Set[RawState],Set[RawArc]) = apply_sys_exp(wts,exp)
					val env_res : (Set[RawState],Set[RawArc]) = apply_env_exp(wts,exp_due_to_environment)
					val new_nodes = sys_res._1 ++ env_res._1

					val post_test = check_post_expansion_validity_test(wts,new_nodes,sys_res._2,conf)
					if (post_test) {
						val updated_labelling = update_wts_labelling(wts,focus,new_nodes,sys_res._2,env_res._2,qos)

						/* FINALLY, the new list of WTS will contain the cloned updated WTS */
						val new_wts = WTSGraph(
							wts.start,                          //initial node
							wts.nodes++new_nodes,               //nodes
							wts.transitions++sys_res._2,        //transitions
							wts.perturbations++env_res._2,      //perturbations
							updated_labelling                   //labelling
						)

						updated_list = new_wts :: updated_list
					} else {
						println("discarded because of POST-TEST")
					}
				} else {
					println("discarded because of PRE-TEST")
				}

			}

			updated_list

		} else if (exp_due_to_environment.nonEmpty) {
				val env_res: (Set[RawState], Set[RawArc]) = apply_env_exp(wts, exp_due_to_environment)
				val new_nodes = env_res._1

				val updated_labelling = update_wts_labelling(wts, focus, new_nodes, Set.empty, env_res._2, qos)

				//val quality = calculate_quality_of_solution(wts,focus,updated_frontier,new_nodes,sys_res._2,env_res._2)

				/* FINALLY, the new list of WTS will contain the cloned updated WTS */
				val new_wts = WTSGraph(
					wts.start, //initial node
					wts.nodes ++ new_nodes, //nodes
					wts.transitions, //transitions
					wts.perturbations ++ env_res._2, //perturbations
					updated_labelling //labelling
				)

			/* FINALLY, the new list of WTS will contain the cloned updated WTS */
			List(new_wts)

		} else {
			List(WTSGraph(
				wts.start, //initial node
				wts.nodes, //nodes
				wts.transitions, //transitions
				wts.perturbations, //perturbations
				update_wts_labelling(wts, focus, Set.empty, Set.empty, Set.empty, qos) //labelling
			))
		}

	}

	private def update_wts_labelling(wts: WTSGraph, focus: RawState, new_nodes: Set[RawState], new_transitions: Set[RawArc], new_perturbations: Set[RawArc], qos : RawState => Float) : WTSLabelling = {
		var updated_node_labelling : Map[RawState,StateLabel] = wts.wts_labelling.labelling

		// Frontier: ALWAYS remove focus (LATER add all new nodes that are not exit nodes)
		var updated_frontier = wts.wts_labelling.frontier - focus

		// Terminal: add focus only IF there are no expansions
		val updated_terminal = if (new_nodes.isEmpty) wts.wts_labelling.terminal + focus else wts.wts_labelling.terminal

		// Labelling:
		// change label of focus node: setting is_frontier to false and is_terminal to true only if there are no expansions
		val focus_label : StateLabel = wts.wts_labelling.labelling(focus)
		val updated_focus_label = focus_label.copy(is_frontier = false, is_terminal = (new_nodes.isEmpty))

		// for each new node, calculate the new goal_supervisor_array, if it is exit_node, the updated_metric
		updated_node_labelling += (focus -> updated_focus_label)
		for (node <- new_nodes) {
			val updated_array : Array[GoalSupervisor] = for (l <- focus_label.sup_array) yield l.getNextSupervisor(node)

			val is_exit = LTLGoalSet.check_exit_node(updated_array)
			val updated_metric : Float = qos(node)
			val updated_label = StateLabel(updated_array,is_exit,!is_exit,is_exit,is_exit,updated_metric)

			//if (!is_exit)
				updated_frontier += node

			updated_node_labelling = updated_node_labelling + (node -> updated_label)
		}

		// Quality: delegate to specific function
		val updated_quality = 0//calculate_quality_of_solution(wts,updated_frontier,updated_node_labelling,new_nodes,new_transitions,new_perturbations)


		WTSLabelling(updated_frontier, updated_terminal, updated_node_labelling, updated_quality)
	}


	/*
	 * This function calculates the quality of the new WTS
	 *
	 * PROVVISORIAMENTE: quality = average of frontier node values
	 */
	private def calculate_quality_of_solution(old_wts: WTSGraph, updated_frontier : Set[RawState], updated_node_labelling: Map[RawState,StateLabel], new_nodes: Set[RawState], new_transition: Set[RawArc], new_perturb: Set[RawArc]): Float = {
		var q : Float = 0
		for (f <- updated_frontier)
			q+=updated_node_labelling(f).metric

		q/updated_frontier.size
	}


	private def apply_sys_exp(wts: WTSGraph, exp : RawExpansion) : (Set[RawState],Set[RawArc]) = {
		var new_nodes : Set[RawState] = Set.empty
		var new_transition : Set[RawArc] = Set.empty

		for (evolution_part <- exp.probtrajectory) {
			val evolution_node = evolution_part.dest

			if (!wts.nodes.contains(evolution_part.dest)) {
				new_nodes = new_nodes + evolution_part.dest
			}


			new_transition = new_transition + RawArc(exp.from, evolution_part.dest, 1, exp.due_to, evolution_part.name)
		}

		(new_nodes,new_transition)
	}

	private def apply_env_exp(wts: WTSGraph, exps : List[RawExpansion]) : (Set[RawState],Set[RawArc]) = {
		var new_nodes : Set[RawState] = Set.empty
		var new_perturb : Set[RawArc] = Set.empty

		for (pert <- exps) {
			for (perturb_part <- pert.probtrajectory) {
				if (!wts.nodes.contains(perturb_part.dest))
					new_nodes = new_nodes + perturb_part.dest

				new_perturb = new_perturb + RawArc(pert.from, perturb_part.dest, perturb_part.probability, pert.due_to, perturb_part.name)
			}
		}

		(new_nodes,new_perturb)
	}

}

/*
case class Node(w : StateOfWorld, interpretation : HerbrandInterpretation) {
	def satisfies(cond : GroundPredicate) : Boolean = {
		interpretation.satisfies(TweetyFormula.fromCond(FOLCondition(GroundLiteral(cond))))
	}

	override def hashCode(): Int = {
		w.hashCode()
	}

	override def equals(obj: Any): Boolean = {
		obj match {
			case that:Node => w == that.w
			case _ => false
		}
	}
}*/
//case class TransitionArc(origin : RawState, destination : RawState, action: SystemAction, scenario_name : String)
case class RawArc(origin : RawState, destination : RawState, probability : Float, action: RawAction, scenario_name : String)


/******* STATE LABELLING ********/

// frontier è una scorciatoia per i nodi ancora da esplorare
// terminal è una scorciatoia per i nodi in cui il goal è soddisfatto
// StateLabel deve contenere anche informazioni tipo:
// nodo terminale successo, nodo violazione, loop senza uscita, loop con uscita

case class WTSLabelling(
	                       frontier : Set[RawState],
	                       terminal : Set[RawState],
	                       labelling : Map[RawState,StateLabel],
	                       quality_of_solution : Float
                       )

case class StateLabel(
	                     sup_array : Array[GoalSupervisor],
	                     is_terminal: Boolean,
	                     is_frontier : Boolean,
	                     is_exit : Boolean,
	                     leads_to_exit : Boolean,
	                     metric : Float)


/******* STATE EVOLUTIONS ********/

class Expansion
//case class SystemExpansion(due_to : SystemAction, from : RawState, trajectory : Array[Evo])
case class RawExpansion(due_to : RawAction, from : RawState, probtrajectory : Array[ProbabilisticEvo])
//case class Evo (name: String, dest : RawState)
case class ProbabilisticEvo (name: String, probability : Float, dest : RawState)

