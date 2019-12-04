package org.icar.pmr_solver.nmc

import org.icar.pmr_solver.{TerminationDescription, TimeTermination}
import org.icar.pmr_solver.high_level_specification.{AbstractCapability, Domain, Problem}
import org.icar.pmr_solver.rete.RETEBuilder
import org.icar.pmr_solver.symbolic_level.{HL2Raw_Map, RawAction, RawGoalModelSupervisor, RawLTL, RawState}

import scala.util.Random

class NMCSolver(val problem: Problem,val domain: Domain) {
	val random = new Random()

	val map = new HL2Raw_Map(domain)

	val I = RawState.factory(map.state_of_world(problem.I.statements),domain.axioms,map)
	val rete = RETEBuilder.factory(domain.axioms,map,I)
	rete.execute

	val specifications: Array[RawLTL] = for (g<-problem.goal_model.goals) yield map.ltl_formula(g)
	val available_actions = (for (a<-problem.actions.sys_action) yield map.system_action(a)).flatten

	val tree = new WTSTree(rete,available_actions,specifications)


	def nmcs(level : Int, node : WTSTreeNode) : (Float,List[Int]) = {
		if (level==0) {
			//println("entering level "+level)
			var focus_node = node
			var delta:Float = 0
			var seq : List[Int] = List.empty

			var depth = 0
			while (delta==0 && focus_node.children.size>0) {
				val i : Int = random.nextInt(focus_node.children.size)
				seq = i :: seq
				focus_node = focus_node.get_child(i)
				depth+=1
				delta = focus_node.r2s - node.r2s
			}
			if (delta<0) focus_node.delta=true

			(delta,seq.reverse)
		} else {

			println("entering level "+level)
			var focus_node = node
			//var return_seq : List[Int] = List.empty
			//var phy : Int = 0
			var seq : List[Int] = List.empty
			var best_score : Float = R2S.Rinf
			var best_index : Int = 0
			var depth = 0

			while (!focus_node.isExit && focus_node.children.size>0 && WTSTreeNode.id<2000) {
				//println("going deeply")
				for (index <- 0 until focus_node.children.size) {
					val temp_node = focus_node.get_child(index)
					val (result,new_seq) = nmcs(level-1,temp_node)
					if (result<best_score) {
						best_score = result
						best_index = index
						//seq = index :: new_seq
					}
				}
				//return_seq = seq(phy) :: return_seq
				seq = best_index :: seq
				focus_node = focus_node.get_child(best_index)
				depth+=1
			}
			(best_score,seq.reverse)
		}
	}

	var solutions = 0
	var iterations=0
	var elapsed : Long= 0

	def mcts(term:TerminationDescription) = {
		val start_timestamp: Long = System.currentTimeMillis
		iterations=0

		standard_mcsp(tree.root,term,start_timestamp)

		val end_timestamp: Long = System.currentTimeMillis
		elapsed = end_timestamp-start_timestamp
	}

	def standard_mcsp(node: WTSTreeNode,term:TerminationDescription,start:Long) : Unit = {
		while (!TerminationDescription.check_termination(term,start,iterations,solutions)) {
			val focus_node = tree_policy (node)
			val delta = simulation_policy (focus_node, node.r2s)
			backpropagation (focus_node, delta)

			//tree.update_wts_file("./data/sps_data/wts_tree.dot")
			iterations += 1
		}
	}

	def recursive_mcsp(node: WTSTreeNode,term:TerminationDescription,start:Long) : Unit = {
		while (!TerminationDescription.check_termination(term,start,iterations,solutions)) {
			val focus_node = tree_policy (node)
			val delta = simulation_policy (focus_node, node.r2s)
			backpropagation (focus_node, delta)

			/* many-levels recursion */
			if (focus_node.delta && !focus_node.isExit)
				standard_mcsp(focus_node,term,start)

			//tree.update_wts_file("./data/sps_data/wts_tree.dot")
			iterations += 1
		}
	}

	def tree_policy(node: WTSTreeNode):WTSTreeNode = {
		var focus = node
		var new_node = node
		while (new_node==node && focus.is_nonterminal) {
			if (focus.is_notfullyexpanded) {
				new_node = expand(focus)
			} else {
				focus = best_child(focus)
			}
		}
		if (new_node != node)
			new_node
		else
			focus
	}

	def expand(node: WTSTreeNode):WTSTreeNode = {
		val untried : Array[Int] = node.untried_actions
		val i : Int = random.nextInt(untried.size)
		val child = node.get_child(untried(i))
		if (child.isExit) solutions += 1
		child
	}

	def simulation_policy(node: WTSTreeNode,r2s_ref:Float):Float = {
		//val r2s_ref = if (node.visit==0) node.parent.r2s else node.r2s
		var delta:Float = if (node.visit==0) r2s_ref-node.r2s else 0

		var focus_node = node
		while (delta==0 && focus_node.is_notfullyexpanded && focus_node.is_nonterminal) {
			focus_node = expand(focus_node)
			delta = r2s_ref-focus_node.r2s
		}
		if (delta>0)
			focus_node.delta=true

		delta
	}

	def backpropagation(node: WTSTreeNode, delta: Float): Unit = {
		//val inc = Math.round(delta/100)
		node.visit += 1

		if (delta>0 && delta>node.win)
			node.win += delta

		if (!node.is_root)
			backpropagation(node.parent,delta)
	}

	def best_child(node: WTSTreeNode): WTSTreeNode = {
		var best = node
		var best_score : Float = -1
		for (some_child<-node.children if some_child.isDefined) {
			val child = some_child.get
			val win = child.win.toFloat
			val visit = child.visit.toFloat
			val R : Float = if (child.visit>0) win/visit else 0
			if (R>best_score) {
				best_score = R
				best = child
			}
		}
		best
	}



	var root_iterator : Int = 0
	var frontier : List[WTSTreeNode] = List()

	def mcts_with_frontier(term:TerminationDescription) : Int = {
		WTSTreeNode.id = 0
		val start_timestamp: Long = System.currentTimeMillis
		var it=0

		while (!TerminationDescription.check_termination(term,start_timestamp,iterations,solutions)){
			if (frontier.isEmpty)
				for (c<-tree.root.children) frontier = tree.root :: frontier

			val focus_node = frontier.head
			frontier = frontier.tail

			if (focus_node.is_root)
				root_iterator += 1

			val deep_node = simulation_until_delta (focus_node, focus_node.r2s)
			backpropagation_with_frontier (deep_node, focus_node.r2s-deep_node.r2s, deep_node.children.size)

			//if (it%20==0)
				//tree.update_wts_file("./data/sps_data/wts_tree.dot")
			frontier = Random.shuffle(frontier)
			it += 1
		}

		val end_timestamp: Long = System.currentTimeMillis
		elapsed = end_timestamp-start_timestamp
		it
	}
	def simulation_until_delta(node: WTSTreeNode,r2s_ref:Float):WTSTreeNode = {
		var delta:Float = 0

		var focus_node = node
		while (delta==0 && focus_node.is_nonterminal) {
			focus_node = expand_or_child(focus_node)
			delta = r2s_ref-focus_node.r2s
		}
		if (delta>0)
			focus_node.delta=true

		focus_node
	}
	def backpropagation_with_frontier(node: WTSTreeNode, delta: Float, magnitude : Int): Unit = {
		node.visit += 1

		if (delta>0){
			node.win += 1

			if (node.delta) {
				val new_nodes_in_frontier : List[WTSTreeNode] = List.fill(magnitude)(node)
				frontier = new_nodes_in_frontier ::: frontier
			}

		}
		if (!node.is_root)
			backpropagation_with_frontier(node.parent,delta,Math.max(Math.round(magnitude/2),1))
	}
	def expand_or_child(node: WTSTreeNode) : WTSTreeNode = {
		val untried : Array[Int] = node.untried_actions
		if (untried.size>0){
			val i : Int = random.nextInt(untried.size)
			val expanded_child = node.get_child(untried(i))
			if (expanded_child.isExit) solutions += 1
			expanded_child
		} else {
			val i : Int = random.nextInt(node.children.size)
			val child = node.get_child(i)
			child
		}
	}






}


