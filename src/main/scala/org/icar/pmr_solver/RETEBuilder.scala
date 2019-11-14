package org.icar.pmr_solver

object RETEBuilder {

	def factory(axioms : Array[Axiom], map:HL2Raw_Map, wi:RawState) : RETE = {
		val rete = new RETE(wi)


		var alpha_list : List[AlphaNode] = List.empty
		var beta_list : List[BetaJoinNode] = List.empty

		var priority:Int = 0

		for (rule<-axioms) {
			rule match {
				case Rule(consequent,antecedent) =>
					var alpha_definitions : List[NodeDefinition] = List.empty

					/* pre-analysis */
					val axioms_for_alpha : List[Predicate] = extract_predicates_for_alpha(antecedent.terms)
					//val axioms_for_condition = List[TestCondition] = antecedent.terms.filter( _.isInstanceOf[TestCondition])

					for (p <- axioms_for_alpha) {
						val alpha = new AlphaNode(map,p,wi)
						rete.root.subnodes = alpha :: rete.root.subnodes

						alpha_definitions = NodeDefinition(alpha,p.terms) :: alpha_definitions
						alpha_list = alpha :: alpha_list
					}

					val last_node : NodeDefinition = create_betas(alpha_definitions)

					//generate the list of Match and Fix for the p-node
					val beta_terms = last_node.terms
					var pn_inf_terms : List[InferenceTerms] = List.empty
					for (cons_term <- consequent.terms)
						cons_term match {
							case term: ConstantTerm => pn_inf_terms = Fix(term) :: pn_inf_terms
							case VariableTerm(name) => pn_inf_terms = Match( search_first_occurrence(beta_terms,name)) :: pn_inf_terms
						}

					// create the pnode
					val rule = new PNode(priority,consequent.functional,pn_inf_terms.reverse,map,rete)
					last_node.node.subnodes = List(rule)

				case _ =>

			}
			priority += 1
		}

		rete.start
		rete
	}

	private def search_first_occurrence(terms: List[Term], var_name: String) : Int = {
		var occ = -1

		var index = 0
		for (t<-terms if occ == -1){
			t match {
				case term: ConstantTerm =>
				case VariableTerm(name) =>
					if (name == var_name) occ = index
			}
			index += 1
		}

		occ
	}


	private def extract_predicates_for_alpha(terms:List[RuleCondition]) : List[Predicate]= {
		var list : List[Predicate] = List.empty
		for (t<-terms)
			t match {
				case PredicateCondition(p) => list = p :: list
				case _ =>
			}
		list
	}
	private def extract_predicates_for_neg_alpha(terms:List[RuleCondition]) : List[Predicate]= {
		var list : List[Predicate] = List.empty
		for (t<-terms)
			t match {
				case NegateCondition(p) => list = p :: list
				case _ =>
			}
		list
	}

	// Given two nodes, it create a join-node also kwnon as BetaNode
	// Note: cases like f(x,x) are alread handled by alpha nodes
	private def create_betas(node_definitions: List[NodeDefinition]): NodeDefinition = {
		/* utility sub-functions */
		def extract_joins(n1: NodeDefinition, n2: NodeDefinition) : List[TermJoin] = {
			var joins : List[TermJoin] = List.empty
			for (i1<-0 until n1.terms.length)
				for (i2<-0 until n2.terms.length)
					if (n1.terms(i1)==n2.terms(i2))
						if (n1.terms(i1).isInstanceOf[VariableTerm])
							joins = TermJoin(i1,i2) :: joins
			joins
		}

		def merge_definitions(a: NodeDefinition, b: NodeDefinition): NodeDefinition = {
			val join_couples = extract_joins(a,b)
			val beta = new BetaJoinNode(a.node,b.node,join_couples)
			NodeDefinition(beta,a.terms:::b.terms)
		}

		/* function starts here */
		if (node_definitions.size==1)
			node_definitions.head
		else {
			/* create beta */
			val first : NodeDefinition = node_definitions.head
			val second : NodeDefinition = node_definitions.tail.head
			val join : NodeDefinition = merge_definitions(first,second)

			/* beta is child of its parent nodes */
			first.node.subnodes = join.node :: first.node.subnodes
			second.node.subnodes = join.node :: second.node.subnodes

			create_betas(join::node_definitions.tail.tail)
		}
	}

}

case class NodeDefinition(node:RETENode,terms: List[Term])
case class JoinDefinition(left:Predicate,left_pos:Int,right:Predicate,right_pos:Int)