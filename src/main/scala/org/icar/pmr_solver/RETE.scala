package org.icar.pmr_solver

import scala.collection.immutable.TreeMap

case class TermMatching(term_list:List[ConstantTerm],dependency:List[Int])
case class Inference(v:RawVar,dependency:List[Int])

abstract class InferenceTerms
case class Match(index:Int) extends InferenceTerms
case class Fix(t:ConstantTerm) extends InferenceTerms



/******* RETE ********/
//case class AgendaItem(v:RawVar,priority:Int)

class RETE {
	var priority_map: TreeMap[Int,RawVar] = TreeMap.empty
	//var agenda : List[AgendaItem] = List.empty
	val root:RootNode = new RootNode

	def add_fact(index:Int) = { root.add_fact(index,root)}
	def retract_fact(index:Int) = { root.retract_fact(index,root)}

	def start = root.start

	def execute = {
		while (!priority_map.isEmpty) {
			execute_step
		}
	}

	def execute_step = {
		val index = priority_map.firstKey
		val v = priority_map(index)

		priority_map = priority_map - index
		//agenda = agenda.filter( _.v!=v)

		add_fact(index)
	}

	def insert_deduction(a:RawVar, p:Int) = {
		//agenda = AgendaItem(a,p) :: agenda
		priority_map = priority_map + (p->a)
	}
	def remove_deduction(a:RawVar) = {
		priority_map = priority_map.filter( _._2 != a )

		/*
		var new_agenda : List[AgendaItem] = List.empty

		for (item <- agenda)
			if (item.v == a)
				priority_map = priority_map - item.priority
			else
				new_agenda = item :: new_agenda

		agenda = new_agenda.reverse
		*/
	}
}


/******* NODES OF THE RETE NETWORK ********/
trait RETENode {
	var subnodes : List[RETENode] = List.empty

	def add_fact(index:Int, source:RETENode) = {}
	def retract_fact(index:Int, source:RETENode) = {}

	def add_assignments(ass:TermMatching, source:RETENode) = {}

	def start : Unit = { subnodes.foreach( _.start ) }
}


class RootNode extends RETENode {
	override def add_fact(index: Int, source:RETENode): Unit = {
		subnodes.foreach( _.add_fact(index,this))
	}

	override def retract_fact(index: Int, source: RETENode): Unit = {
		subnodes.foreach( _.retract_fact(index,this))

	}
}

class AlphaNode(domain:HL2Raw_Map, pred:Predicate, wi:RawState) extends RETENode {
	val init_list = domain.all_matching_vars(pred)

	var tokens : Map[RawVar,Boolean] = Map.empty

	init

	def init = {
		for (v<-init_list)
			setToken(v.index,wi.state(v.index))
	}

	override def start: Unit = {
		//subnodes.foreach( _.start )
		for (i<-init_list if wi.state(i.index)==true) {
			val p : GroundPredicate =domain.inverse(i.index)
			val matching = TermMatching(p.terms, List(i.index))
			subnodes.foreach( _.add_assignments(matching,this) )
		}
	}

	def setToken(i:Int,s:Boolean) = {tokens += (RawVar(i)->s)}

	override def add_fact(index: Int, source:RETENode): Unit = {
		val v = RawVar(index)
		if (tokens.contains(v) && tokens(v)==false) {
			println("**alpha interested**")
			tokens += (v->true)

			val p : GroundPredicate =domain.inverse(index)
			val matching = TermMatching(p.terms, List(index))
			subnodes.foreach( _.add_assignments(matching,this) )
		}
	}

	override def retract_fact(index: Int, source: RETENode): Unit = {
		val v = RawVar(index)
		if (tokens.contains(v) && tokens(v)==true) {
			tokens += (v->false)
			val p : GroundPredicate =domain.inverse(index)
			subnodes.foreach( _.retract_fact(index,this) )
		}
	}
}

class AlphaNegatedNode(domain:HL2Raw_Map, pred:Predicate, wi:RawState) extends RETENode {
	val init_list = domain.all_matching_vars(pred)

	var tokens : Map[RawVar,Boolean] = Map.empty

	init

	def init = {
		for (v<-init_list)
			setToken(v.index,wi.state(v.index))
	}

	override def start: Unit = {
		//subnodes.foreach( _.start )
		for (i<-init_list if wi.state(i.index)==false) {
			val p : GroundPredicate =domain.inverse(i.index)
			val matching = TermMatching(p.terms, List(i.index))
			subnodes.foreach( _.add_assignments(matching,this) )
		}
	}

	def setToken(i:Int,s:Boolean) = {tokens += (RawVar(i)->s)}

	override def add_fact(index: Int, source:RETENode): Unit = {
		val v = RawVar(index)
		if (tokens.contains(v) && tokens(v)==false) {
			tokens += (v->true)

			val p : GroundPredicate =domain.inverse(index)
			subnodes.foreach( _.retract_fact(index,this) )
		}
	}

	override def retract_fact(index: Int, source: RETENode) : Unit = {
		val v = RawVar(index)
		if (tokens.contains(v) && tokens(v)==true) {
			tokens += (v->false)

			println("**alpha interested**")
			val p : GroundPredicate =domain.inverse(index)
			val matching = TermMatching(p.terms, List(index))
			subnodes.foreach( _.add_assignments(matching,this) )

		}
	}

}

class BetaConditionNode(condition:ConstantTerm=>Boolean, arg_num:Int) extends RETENode {

	override def add_assignments(ass: TermMatching, source: RETENode): Unit = {
		println(s"**beta-condition interested $ass**")
		val term = ass.term_list(arg_num)
		if (condition(term))
			subnodes.foreach( _.add_assignments(ass,this) )

	}

	override def retract_fact(index: Int, source: RETENode): Unit = {
		subnodes.foreach( _.retract_fact(index,this) )
	}
}

// Note: so far it does not manage multiple joins: f(x,y) ^ g(x,y)
class BetaJoinNode(l:RETENode, left_join:Int, r:RETENode, right_join:Int) extends RETENode {
	var list_of_left_matching : List[TermMatching] = List.empty
	var list_of_right_matching : List[TermMatching] = List.empty

	private def add_left_matching(m:TermMatching) = {list_of_left_matching = m :: list_of_left_matching}
	private def add_right_matching(m:TermMatching) = {list_of_right_matching = m :: list_of_right_matching}

	override def add_assignments(ass: TermMatching, source: RETENode): Unit = {

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

	override def retract_fact(index: Int, source: RETENode): Unit = {
		if (source ==l)
			list_of_left_matching = list_of_left_matching.filter( !_.dependency.contains(index) )
		else if (source==r)
			list_of_right_matching = list_of_right_matching.filter( !_.dependency.contains(index) )

		subnodes.foreach( _.retract_fact(index,this))
	}

}

class PNode(priority: Int, functor:String, args:List[InferenceTerms], domain:HL2Raw_Map, agenda:RETE) extends RETENode {
	var inference_list : List[Inference] = List.empty

	override def add_assignments(ass: TermMatching, source: RETENode): Unit = {
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
		agenda.insert_deduction(inference,priority)
	}

	override def retract_fact(index: Int, source: RETENode): Unit = {
		var new_inference_list : List[Inference] = List.empty

		for (inf<-inference_list) {
			if (inf.dependency.contains(index)) {
				val variable = inf.v
				val predicate = domain.inverse(variable.index)

				println(s"** retract $predicate => $variable")
				agenda.remove_deduction(variable)
			} else {
				new_inference_list = inf :: new_inference_list
			}
		}

		inference_list = new_inference_list
	}

}


