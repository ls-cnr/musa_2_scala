package org.icar.pmr_solver

case class Matches(set:List[ConstantTerm])
abstract class InferenceTerms
case class Match(index:Int) extends InferenceTerms
case class Fix(t:ConstantTerm) extends InferenceTerms

trait ReteNode {
	var subnodes : List[ReteNode] = List.empty

	def add_fact(index:Int, source:ReteNode) = {}
	def add_assignments(ass:Matches, source:ReteNode) = {}
}

class RootNode extends ReteNode {
	override def add_fact(index: Int, source:ReteNode): Unit = {
		subnodes.foreach( _.add_fact(index,this))
	}
}

/*class FactNode extends ReteNode*/

class AlphaNode(domain:HL2Raw_Map) extends ReteNode with NodeMemory {
	override def add_fact(index: Int, source:ReteNode): Unit = {
		val v = RawVar(index)
		if (tokens.contains(v) && tokens(v)==false) {
			println("**alpha interested**")
			tokens += (v->true)

			val p : GroundPredicate =domain.inverse(index)
			subnodes.foreach( _.add_assignments(Matches(p.terms),this) )
		}

	}
}
class NegatedAlphaNode extends ReteNode with NodeMemory

class BetaOneInputNode extends ReteNode with NodeMemory

class BetaTwoInputNode(l:ReteNode,left_join:Int,r:ReteNode,right_join:Int) extends ReteNode with JoinMemory {

	override def add_assignments(ass: Matches, source: ReteNode): Unit = {
		if (source==l) {
			println("**beta interested (from left)**")
			left = ass :: left
			join_from_left_to_right(ass)
		} else if (source==r) {
			println("**beta interested (from right)**")
			right = ass :: right
			join_from_right_to_left(ass)
		}

	}

	def join_from_left_to_right(left : Matches) : Unit = {

		val left_term = left.set(left_join)
		println(s"**left=$left with $left_term **")

		for (r<-right) {
			val right_term = r.set(right_join)
			println(s"**right=$right with $right_term **")
			if (left_term==right_term) {
				println("**matches**")
				subnodes.foreach( _.add_assignments(Matches(left.set ::: r.set),this) )
			}
		}
	}

	def join_from_right_to_left(right : Matches) : Unit = {

		val right_term = right.set(left_join)
		println(s"**right=$right with $right_term **")

		for (l<-left) {
			val left_term = l.set(left_join)
			println(s"**left=$l with $left_term **")
			if (left_term==right_term) {
				println("**matches**")
				subnodes.foreach( _.add_assignments(Matches(l.set ::: right.set),this) )
			}
		}
	}
}

class PNode(functor:String, args:List[InferenceTerms]) extends ReteNode with NodeMemory {

	override def add_assignments(ass: Matches, source: ReteNode): Unit = {

	}

}



trait NodeMemory {
	var tokens : Map[RawVar,Boolean] = Map.empty
	def setToken(i:Int,s:Boolean) = {tokens += (RawVar(i)->s)}
}

trait JoinMemory {
	var left : List[Matches] = List.empty
	var right : List[Matches] = List.empty
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
		alpha_available.subnodes = List(beta_ava_rece)
		alpha_received.subnodes = List(beta_ava_rece)

		val r1 = new PNode("document", List(Match(0), Fix(AtomTerm("ready"))))


		root.subnodes = List(alpha_available,alpha_received)

		println("enter: 18")
		root.add_fact(18,root)
		println("enter: 17")
		root.add_fact(17,root)

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