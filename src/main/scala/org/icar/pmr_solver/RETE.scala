package org.icar.pmr_solver

case class TermMatching(term_list:List[ConstantTerm],dependency:List[Int])
case class Inference(v:RawVar,dependency:List[Int])

abstract class InferenceTerms
case class Match(index:Int) extends InferenceTerms
case class Fix(t:ConstantTerm) extends InferenceTerms




trait ReteNode {
	var subnodes : List[ReteNode] = List.empty

	def add_fact(index:Int, source:ReteNode) = {}
	def retract_fact(index:Int, source:ReteNode) = {}

	def add_assignments(ass:TermMatching, source:ReteNode) = {}
}

class RootNode extends ReteNode {
	override def add_fact(index: Int, source:ReteNode): Unit = {
		subnodes.foreach( _.add_fact(index,this))
	}

	override def retract_fact(index: Int, source: ReteNode): Unit = {
		subnodes.foreach( _.retract_fact(index,this))

	}
}

class AlphaNode(domain:HL2Raw_Map) extends ReteNode {
	var tokens : Map[RawVar,Boolean] = Map.empty
	def setToken(i:Int,s:Boolean) = {tokens += (RawVar(i)->s)}

	override def add_fact(index: Int, source:ReteNode): Unit = {
		val v = RawVar(index)
		if (tokens.contains(v) && tokens(v)==false) {
			println("**alpha interested**")
			tokens += (v->true)

			val p : GroundPredicate =domain.inverse(index)
			val matching = TermMatching(p.terms, List(index))
			subnodes.foreach( _.add_assignments(matching,this) )
		}
	}

	override def retract_fact(index: Int, source: ReteNode): Unit = {
		val v = RawVar(index)
		if (tokens.contains(v) && tokens(v)==true) {
			tokens += (v->false)
			val p : GroundPredicate =domain.inverse(index)
			subnodes.foreach( _.retract_fact(index,this) )
		}
	}
}

/* to be implemented */
class NegatedAlphaNode extends ReteNode
class BetaOneInputNode extends ReteNode

class BetaTwoInputNode(l:ReteNode,left_join:Int,r:ReteNode,right_join:Int) extends ReteNode {
	var list_of_left_matching : List[TermMatching] = List.empty
	var list_of_right_matching : List[TermMatching] = List.empty

	private def add_left_matching(m:TermMatching) = {list_of_left_matching = m :: list_of_left_matching}
	private def add_right_matching(m:TermMatching) = {list_of_right_matching = m :: list_of_right_matching}

	override def add_assignments(ass: TermMatching, source: ReteNode): Unit = {

		/* sub-functions */
		def join_from_left_to_right(left_matching : TermMatching) : Unit = {

			val left_term = left_matching.term_list(left_join)
			println(s"**left=$left_matching with $left_term **")

			for (right_matching<-list_of_right_matching) {
				val right_term = right_matching.term_list(right_join)
				println(s"**right=$list_of_right_matching with $right_term **")
				if (left_term==right_term) {
					println("**matches**")
					val join_term_list = left_matching.term_list ::: right_matching.term_list
					val join_dependency = left_matching.dependency ::: right_matching.dependency
					val join_matching = TermMatching(join_term_list,join_dependency)
					subnodes.foreach( _.add_assignments(join_matching,this) )
				}
			}
		}

		def join_from_right_to_left(right_matching : TermMatching) : Unit = {

			val right_term = right_matching.term_list(left_join)
			println(s"**right=$right_matching with $right_term **")

			for (left_matching<-list_of_left_matching) {
				val left_term = left_matching.term_list(left_join)
				println(s"**left=$left_matching with $left_term **")
				if (left_term==right_term) {
					println("**matches**")
					val join_term_list = left_matching.term_list ::: right_matching.term_list
					val join_dependency = left_matching.dependency ::: right_matching.dependency
					val join_matching = TermMatching(join_term_list,join_dependency)
					subnodes.foreach( _.add_assignments(join_matching,this) )
				}
			}
		}

		/* function starts here */
		if (source==l) {
			println("**beta interested (from left)**")
			add_left_matching(ass)
			join_from_left_to_right(ass)
		} else if (source==r) {
			println("**beta interested (from right)**")
			add_right_matching(ass)
			join_from_right_to_left(ass)
		}

	}

	override def retract_fact(index: Int, source: ReteNode): Unit = {
		if (source ==l)
			list_of_left_matching = list_of_left_matching.filter( !_.dependency.contains(index) )
		else if (source==r)
			list_of_right_matching = list_of_right_matching.filter( !_.dependency.contains(index) )

		subnodes.foreach( _.retract_fact(index,this))
	}

}

class PNode(functor:String, args:List[InferenceTerms], domain:HL2Raw_Map) extends ReteNode {
	var inference_list : List[Inference] = List.empty

	override def add_assignments(ass: TermMatching, source: ReteNode): Unit = {
		var terms : List[ConstantTerm] = List.empty
		for (a<-args) {
			a match {
				case Match(index) =>
					val term = ass.term_list(index)
					terms = term :: terms

				case Fix(term) => terms = term :: terms

			}
		}

		val p = GroundPredicate( functor, terms.reverse )
		val inference = RawVar(domain.direct(p))
		inference_list = Inference(inference,ass.dependency) :: inference_list
		println(s"** deduced $p => $inference")
	}

	override def retract_fact(index: Int, source: ReteNode): Unit = {
		var new_inference_list : List[Inference] = List.empty

		for (inf<-inference_list) {
			if (inf.dependency.contains(index)) {
				val variable = inf.v
				val predicate = domain.inverse(variable.index)
				println(s"** retract $predicate => $variable")
			} else {
				new_inference_list = inf :: new_inference_list
			}
		}

		inference_list = new_inference_list
	}

}



object TestRete extends App {
	ReteBuilder.ready_document
}


object ReteBuilder {
	def ready_document : RootNode = {
		val dom_types : Array[DomainType] = Array(
			EnumerativeDomainType("doc_type",Array("issue_list","paper","tech_rep")),
			EnumerativeDomainType("doc_state",Array("ready","available","received","registered","worked","accepted","rejected","to_revise"))
		)

		val preds : Array[DomainPredicate] = Array(
			DomainPredicate("document",List(
				DomainVariable("TYPE","doc_type"),
				DomainVariable("STATE","doc_state")
			))
		)

		val domain = Domain(preds,dom_types,Array.empty,_=>0)
		val map = new HL2Raw_Map(domain)

		val aval_pred = Predicate("document", List(VariableTerm("TYPE"), AtomTerm("available")))
		val available_match_list = map.all_matching_vars(aval_pred)

		val rece_pred = Predicate("document", List(VariableTerm("TYPE"), AtomTerm("received")))
		val received_match_list = map.all_matching_vars(rece_pred)

		val root = new RootNode

		val alpha_available : AlphaNode = new AlphaNode(map)
		for (v<-available_match_list)
			alpha_available.setToken(v.index,false)

		val alpha_received : AlphaNode = new AlphaNode(map)
		for (v<-received_match_list)
			alpha_received.setToken(v.index,false)

		val beta_ava_rece = new BetaTwoInputNode(alpha_available,0,alpha_received,0)

		val r1 = new PNode("document", List(Match(0), Fix(AtomTerm("ready"))), map)

		root.subnodes = List(alpha_available,alpha_received)
		alpha_available.subnodes = List(beta_ava_rece)
		alpha_received.subnodes = List(beta_ava_rece)
		beta_ava_rece.subnodes = List(r1)


		println("new fact: 18")
		root.add_fact(18,root)
		println("new fact: 17")
		root.add_fact(17,root)


		println("remove fact: 18")
		root.retract_fact(18,root)

		root
	}

	def factory(axioms : Array[Axiom]) : RootNode = {
		val root = new RootNode
		var alpha_list : List[AlphaNode] = List.empty
		var beta_list : List[BetaTwoInputNode] = List.empty

		for (rule<-axioms) {
			rule match {
				case Rule(rhs,lhs) =>
					for (t<-lhs.terms) {
						t match {
							case PredicateCondition(p) => /* alpha node */

							case NegateCondition(p) => /* negated alpha node */

							case EqualTestCondition() => /* one-input beta node */

							case LessTestCondition() => /* one-input beta node */

							case GreaterTestCondition() => /* one-input beta node */

						}

					}

				case _ =>

			}
		}




		root
	}
}