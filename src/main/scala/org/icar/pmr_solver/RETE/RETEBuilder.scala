package org.icar.pmr_solver.RETE

import org.icar.pmr_solver._

object RETEBuilder {
	case class NodeDefinition(node:RETENode,terms: List[Term])
	case class JoinDefinition(left:Predicate,left_pos:Int,right:Predicate,right_pos:Int)

	def factory(axioms : Array[Axiom], map:HL2Raw_Map, wi:RawState) : RETE = {
		val rete = new RETE(wi)
		var alpha_counter:Int=0
		var beta_counter:Int=0
		var p_counter:Int=0
		var alpha_list : List[AlphaNode] = List.empty
		var beta_list : List[BetaJoinNode] = List.empty
		var priority:Int = 0


		/* utility functions */
		def create_betas(node_definitions: List[NodeDefinition]): NodeDefinition = {
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

			val beta = new BetaJoinNode(rete,beta_counter,a.node,b.node,join_couples)
			val beta_memory = new BetaMemory
			rete.memory.beta_memory += (beta_counter->beta_memory)
			beta_counter += 1

			NodeDefinition(beta,a.terms:::b.terms)
		}
		/* end utility functions */


		/* function starts here */
		for (rule<-axioms) {
			rule match {
				case Rule(consequent,antecedent) =>
					var alpha_definitions : List[NodeDefinition] = List.empty

					/* pre-analysis */
					val axioms_for_alpha : List[Predicate] = extract_predicates_for_alpha(antecedent.terms)
					//val axioms_for_condition = List[TestCondition] = antecedent.terms.filter( _.isInstanceOf[TestCondition])

					for (p <- axioms_for_alpha) {
						/* init the alpha / alpha memory */
						val init_list = map.all_matching_vars(p)
						val alpha = new AlphaNode(rete,alpha_counter,init_list,map)

						val alpha_memory = new AlphaMemory(Map.empty)
						for (v<-init_list)
							alpha_memory.setToken(v.index,wi.state(v.index))

						rete.memory.alpha_memory += (alpha_counter -> alpha_memory)
						rete.root.subnodes = alpha :: rete.root.subnodes

						alpha_definitions = NodeDefinition(alpha,p.terms) :: alpha_definitions
						alpha_list = alpha :: alpha_list

						alpha_counter += 1
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
					val rule = new PNode(rete,p_counter,priority,consequent.functional,pn_inf_terms.reverse,map)
					val pmemory = new PMemory()
					rete.memory.p_memory += (p_counter->pmemory)
					p_counter += 1

					last_node.node.subnodes = List(rule)



				case _ =>

			}
			priority += 1
		}

		rete.start(wi)
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
	// Given two nodes, it create a join-node also kwnon as BetaNode
	// Note: cases like f(x,x) are alread handled by alpha nodes

}
