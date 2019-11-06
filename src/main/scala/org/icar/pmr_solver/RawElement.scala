package org.icar.pmr_solver

case class RawState(state:Array[Boolean]) {
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

	lazy val compact_description = calculate_compact_description

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

	override def toString: String = compact_description

	override def hashCode(): Int = compact_description.hashCode

	override def equals(obj: Any): Boolean = {
		obj match {
			case that:RawState => compact_description == that.compact_description
			case _ => false
		}
	}
}


object RawState {

	def factory(core:Array[Boolean],axioms:Array[axiom]) : RawState = {
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
}




case class RawAction(id:String,pre:RawPredicate,effects:Array[RawEvolution])
case class RawEvolution(name : String, probability : Float, evo : Array[RawEvoOperator])

abstract class RawEvoOperator
case class RawAdd(add : RawVar) extends RawEvoOperator
case class RawRem(rmv : RawVar) extends RawEvoOperator



