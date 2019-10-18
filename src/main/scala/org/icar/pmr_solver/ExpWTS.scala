package org.icar.pmr_solver

import net.sf.tweety.logics.fol.semantics.HerbrandInterpretation
import org.icar.musa.context.StateOfWorld

/******* WTS ********/

case class ExpWTS(
	                 start : Node,
	                 nodes : Set[Node],
	                 transitions : Set[TransitionArc],
	                 perturbations : Set[PerturbationArc],

	                 wts_labelling : WTSLabelling

                 ) {

	def node_is_terminal(node: Node) : Boolean = wts_labelling.terminal.contains(node)

	def isFullSolution : Boolean = {
		var full=true

		for (s <- wts_labelling.terminal) {
			if (!LTLGoalSet.check_exit_node(wts_labelling.labelling(s).sup_array))
				full=false
		}

		full
	}
	def isPartialSolution : Boolean = !isFullSolution

}

object ExpWTS {

	def update_wts(wts:ExpWTS, focus : Node, exp_due_to_system: List[SystemExpansion], exp_due_to_environment: List[EnvironmentExpansion],qos : Node => Float) : List[ExpWTS] = {

		if (!wts.nodes.contains(focus)) {

			List(wts)

		} else if (!wts.wts_labelling.labelling(focus).is_exit && exp_due_to_system.nonEmpty) {

			var updated_list : List[ExpWTS] = List.empty

			for (exp <- exp_due_to_system) {

				val sys_res : (Set[Node],Set[TransitionArc]) = apply_sys_exp(wts,exp)
				val env_res : (Set[Node],Set[PerturbationArc]) = apply_env_exp(wts,exp_due_to_environment)
				val new_nodes = sys_res._1 ++ env_res._1

				val updated_labelling = update_wts_labelling(wts,focus,new_nodes,sys_res._2,env_res._2,qos)

				/* FINALLY, the new list of WTS will contain the cloned updated WTS */
				val new_wts = ExpWTS(
					wts.start,                          //initial node
					wts.nodes++new_nodes,               //nodes
					wts.transitions++sys_res._2,        //transitions
					wts.perturbations++env_res._2,      //perturbations
					updated_labelling                   //labelling
				)

				updated_list = new_wts :: updated_list
			}

			updated_list

		} else {

			val env_res : (Set[Node],Set[PerturbationArc]) = apply_env_exp(wts,exp_due_to_environment)
			val new_nodes = env_res._1

			val updated_labelling = update_wts_labelling(wts,focus,new_nodes,Set.empty,env_res._2,qos)

			//val quality = calculate_quality_of_solution(wts,focus,updated_frontier,new_nodes,sys_res._2,env_res._2)

			/* FINALLY, the new list of WTS will contain the cloned updated WTS */
			val new_wts = ExpWTS(
				wts.start,                          //initial node
				wts.nodes++new_nodes,               //nodes
				wts.transitions,        //transitions
				wts.perturbations++env_res._2,      //perturbations
				updated_labelling                   //labelling
			)

			/* FINALLY, the new list of WTS will contain the cloned updated WTS */
			List(new_wts)

		}

	}

	private def update_wts_labelling(wts: ExpWTS, focus: Node, new_nodes: Set[Node], new_transitions: Set[TransitionArc], new_perturbations: Set[PerturbationArc],qos : Node => Float) : WTSLabelling = {
		var updated_node_labelling : Map[Node,StateLabel] = wts.wts_labelling.labelling

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
	private def calculate_quality_of_solution(old_wts: ExpWTS, updated_frontier : Set[Node], updated_node_labelling: Map[Node,StateLabel], new_nodes: Set[Node], new_transition: Set[TransitionArc], new_perturb: Set[PerturbationArc]): Float = {
		var q : Float = 0
		for (f <- updated_frontier)
			q+=updated_node_labelling(f).metric

		q/updated_frontier.size
	}


	private def apply_sys_exp(wts: ExpWTS,exp : SystemExpansion) : (Set[Node],Set[TransitionArc]) = {
		var new_nodes : Set[Node] = Set.empty
		var new_transition : Set[TransitionArc] = Set.empty

		for (evolution_part <- exp.trajectory) {
			val evolution_node = evolution_part.dest

			if (!wts.nodes.contains(evolution_part.dest)) {
				new_nodes = new_nodes + evolution_part.dest
			}


			new_transition = new_transition + TransitionArc(exp.from, evolution_part.dest, exp.due_to, evolution_part.name)
		}

		(new_nodes,new_transition)
	}

	private def apply_env_exp(wts: ExpWTS, exps : List[EnvironmentExpansion]) : (Set[Node],Set[PerturbationArc]) = {
		var new_nodes : Set[Node] = Set.empty
		var new_perturb : Set[PerturbationArc] = Set.empty

		for (pert <- exps) {
			for (perturb_part <- pert.probtrajectory) {
				if (!wts.nodes.contains(perturb_part.dest))
					new_nodes = new_nodes + perturb_part.dest

				new_perturb = new_perturb + PerturbationArc(pert.from, perturb_part.dest, perturb_part.probability, pert.due_to, perturb_part.name)
			}
		}

		(new_nodes,new_perturb)
	}

}

case class Node(w : StateOfWorld, interpretation : HerbrandInterpretation)
case class TransitionArc(origin : Node, destination : Node, action: SystemAction, scenario_name : String)
case class PerturbationArc(origin : Node, destination : Node, probability : Float, env_action: EnvironmentAction, scenario_name : String)

case class WTSLabelling(
	                       frontier : Set[Node],
	                       terminal : Set[Node],
	                       labelling : Map[Node,StateLabel],
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
case class SystemExpansion(due_to : SystemAction, from : Node, trajectory : Array[Evo])
case class EnvironmentExpansion(due_to : EnvironmentAction, from : Node, probtrajectory : Array[ProbabilisticEvo])
case class Evo (name: String, dest : Node)
case class ProbabilisticEvo (name: String, probability : Float, dest : Node)

