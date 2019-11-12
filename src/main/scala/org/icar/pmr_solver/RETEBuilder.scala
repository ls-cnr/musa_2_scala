package org.icar.pmr_solver

object RETEBuilder {




	// Note: f(x,x) is alread handled by alpha node
	private def create_betas(node_definitions: List[NodeDefinition]): NodeDefinition = {
		/* utilities */
		def joinable(a: NodeDefinition, b: NodeDefinition): Boolean = {???}
		
		def merge_definitions(a: NodeDefinition, b: NodeDefinition): NodeDefinition = {
			if (joinable(a,b)) {
				// join with BetaJoinNode
				val pos_a = -1
				val pos_b = -1
				val beta = new BetaJoinNode(a.node,pos_a,b.node,pos_b)
				NodeDefinition(beta,a.terms::b.terms)
			} else {
				// join with BetaDummyNode

				val beta = new BetaJoinNode(a.node,-1,b.node,-1)
				NodeDefinition(beta,a.terms::b.terms)
			}
		}

		/* function starts here */
		if (node_definitions.size==1)
			node_definitions.head
		else {
			val first = node_definitions.head
			val second = node_definitions.tail.head
			val join : NodeDefinition = merge_definitions(first,second)
			create_betas(join::node_definitions.tail.tail)
		}
	}

	def factory(axioms : Array[Axiom], map:HL2Raw_Map, wi:RawState) : RETE = {
		val rete = new RETE


		var alpha_list : List[AlphaNode] = List.empty
		var beta_list : List[BetaJoinNode] = List.empty

		var priority:Int = 0

		for (rule<-axioms) {
			rule match {
				case Rule(consequent,antecedent) =>
					var alpha_definitions : List[NodeDefinition] = List.empty

					/* pre-analysis */
					val axioms_for_alpha : List[Predicate] = extract_predicates_for_alpha(antecedent.terms)
					val axioms_for_neg_alpha : List[Predicate] = extract_predicates_for_neg_alpha(antecedent.terms)
					//val axioms_for_condition = List[TestCondition] = antecedent.terms.filter( _.isInstanceOf[TestCondition])
					//val join_couples = List[JoinDefinition] = ???

					for (p <- axioms_for_alpha) {
						val alpha = new AlphaNode(map,p,wi)
						rete.root.subnodes = alpha :: rete.root.subnodes

						alpha_definitions = NodeDefinition(alpha,p.terms) :: alpha_definitions
						alpha_list = alpha :: alpha_list
					}
					for (p <- axioms_for_neg_alpha) {
						val alpha = new AlphaNegatedNode(map,p,wi)
						rete.root.subnodes = alpha :: rete.root.subnodes

						alpha_definitions = NodeDefinition(alpha,p.terms) :: alpha_definitions
						alpha_list = alpha :: alpha_list
					}

					val last_node : RETENode = create_betas(alpha_definitions)

					//TODO: capire come creare la lista di Match e Fix; per adesso lista vuota
					val rule = new PNode(priority,consequent.functional,List.empty,map,rete)
					last_node.subnodes = List(rule)

				case _ =>

			}
			priority += 1
		}




		rete
	}

	def extract_predicates_for_alpha(terms:List[RuleCondition]) : List[Predicate]= {
		var list : List[Predicate] = List.empty
		for (t<-terms)
			t match {
				case PredicateCondition(p) => list = p :: list
				case _ =>
			}
		list
	}
	def extract_predicates_for_neg_alpha(terms:List[RuleCondition]) : List[Predicate]= {
		var list : List[Predicate] = List.empty
		for (t<-terms)
			t match {
				case NegateCondition(p) => list = p :: list
				case _ =>
			}
		list
	}

}

case class NodeDefinition(node:RETENode,terms: List[Term])
case class JoinDefinition(left:Predicate,left_pos:Int,right:Predicate,right_pos:Int)