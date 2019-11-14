package org.icar.pmr_solver

/******* ACTION ********/
case class RawAction(id:String,pre:RawPredicate,effects:Array[RawEvolution])
case class RawEvolution(name : String, probability : Float, evo : Array[RawEvoOperator])

abstract class RawEvoOperator
case class RawAdd(add : RawVar) extends RawEvoOperator
case class RawRem(rmv : RawVar) extends RawEvoOperator


/******* STATE ********/
case class RawState(state:Array[Boolean]) {
	lazy val compact_description = calculate_compact_description

	/*
	def touch_var(index:Int,value:Boolean) = {
		state(index)=value
		compact_description = calculate_compact_description
	}
	*/


	def satisfies(v:RawVar):Boolean = state(v.index)
	def satisfies(p:RawPredicate) : Boolean = {
		p match {
			case RawVar(i) => state(i)
			case RawTT() => true
			case RawFF() => false
			case RawConj(l,r) =>
				val a = l.asInstanceOf[RawPredicate]
				val b = r.asInstanceOf[RawPredicate]
				satisfies(a)&&satisfies(b)
			case RawDisj(l,r) =>
				val a = l.asInstanceOf[RawPredicate]
				val b = r.asInstanceOf[RawPredicate]
				satisfies(a)||satisfies(b)
			case RawNeg(op) =>
				! satisfies(op.asInstanceOf[RawPredicate])
			case RawImpl(l,r) =>
				val a = l.asInstanceOf[RawPredicate]
				val b = r.asInstanceOf[RawPredicate]
				satisfies(RawNeg(a))||satisfies(b)
			case RawIff(l,r) =>
				val a = l.asInstanceOf[RawPredicate]
				val b = r.asInstanceOf[RawPredicate]
				(satisfies(a)&&satisfies(b)) || (satisfies(RawNeg(a))&&satisfies(RawNeg(b)))
			case _ => false
		}
	}

	override def toString: String = compact_description
	override def hashCode(): Int = compact_description.hashCode
	override def equals(obj: Any): Boolean = {
		obj match {
			case that:RawState => compact_description == that.compact_description
			case _ => false
		}
	}

	private def calculate_compact_description = {
		var first = true
		var s ="["
		for (i<-0 until state.length)
			if (state(i)) {
				if (first)
					first = false
				else
					s+=","
				s+="x"+i
			}
		s+"]"
	}
}

object RawState {

	def empty(size:Int):RawState = RawState( Array.fill[Boolean](size)(false) )

	def factory(core:Array[Boolean],axioms:Array[Axiom]) : RawState = {

		//by now (later implement a RETE algorithm)

		RawState(core)
	}

	def extend(base:RawState,evo:RawEvolution) : RawState = {
		val ext: Array[Boolean] = base.state.clone()

		for (op <- evo.evo)
			op match {
				case RawAdd(RawVar(i)) => ext(i)=true
				case RawRem(RawVar(i)) => ext(i)=false
			}

		RawState(ext)
	}

	def touch(current:RawState,index:Int,value:Boolean) : RawState = {
		if (current.state(index)==value)
			current
		else {
			val ext: Array[Boolean] = current.state.clone()
			ext(index)=value
			RawState(ext)
		}
	}

}


/******* STATE EVOLUTIONS ********/
class Expansion
case class RawExpansion(due_to : RawAction, from : RawState, probtrajectory : Array[ProbabilisticEvo])
case class ProbabilisticEvo (name: String, probability : Float, dest : RawState, supervisor : RawGoalModelSupervisor)




