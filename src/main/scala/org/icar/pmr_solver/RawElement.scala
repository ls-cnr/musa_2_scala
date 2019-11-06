package org.icar.pmr_solver



case class RawState(state:Array[Boolean]) {
	lazy val compact_description = calculate_compact_description

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




case class RawLTLSupervisor(success : Boolean,next_ltl : RawLTL)




case class RawGoalModelSupervisor(sups : Array[RawLTLSupervisor]) {

	def check_exit_node : Boolean = {
		var exit=true
		for (s <- sups)
			if (s.next_ltl != RawTT() || !s.success)
				exit = false

		exit
	}

	def getNext(state:RawState) : RawGoalModelSupervisor = {
		val array = for (ltl<-sups) yield compute_next(state,ltl.next_ltl)
		RawGoalModelSupervisor(array)
	}

	private def compute_next(state : RawState, formula : RawLTL) : RawLTLSupervisor = {

		formula match {
			case RawTT() => RawLTLSupervisor(true, RawTT())
			case RawNeg(RawTT()) => compute_next(state,RawFF())

			case RawFF() => RawLTLSupervisor(false, RawFF())
			case RawNeg(RawFF()) => compute_next(state,RawTT())

			case RawVar(i) =>
				if (state satisfies RawVar(i))
					RawLTLSupervisor(true, RawTT())
				else
					RawLTLSupervisor(false, RawFF())

			case RawNeg(RawVar(i)) =>
				if (state satisfies RawVar(i))
					RawLTLSupervisor(false, RawFF())
				else
					RawLTLSupervisor(true, RawTT())

			case RawConj(l, r) =>
				val a = l.asInstanceOf[RawLTL]
				val b = r.asInstanceOf[RawLTL]
				val next_a = compute_next(state,a)
				val next_b = compute_next(state,b)

				if (next_a.next_ltl != RawTT() && next_b.next_ltl != RawTT())
					RawLTLSupervisor(next_a.success && next_b.success, RawConj(next_a.next_ltl, next_b.next_ltl))

				else if (next_b.next_ltl != RawTT())
					RawLTLSupervisor(next_a.success && next_b.success, next_b.next_ltl)

				else if (next_a.next_ltl != RawTT())
					RawLTLSupervisor(next_a.success && next_b.success, next_a.next_ltl)

				else
					RawLTLSupervisor(next_a.success && next_b.success, RawTT())

			case RawNeg(RawConj(a, b)) => compute_next(state,RawDisj(RawNeg(a), RawNeg(b)))

			case RawDisj(l, r) =>
				val a = l.asInstanceOf[RawLTL]
				val b = r.asInstanceOf[RawLTL]
				val next_a = compute_next(state,a)
				val next_b = compute_next(state,b)
				val a_test = next_a.success
				val next_a_formula = next_a.next_ltl
				val b_test = next_b.success
				val next_b_formula = next_b.next_ltl

				if (next_a_formula != RawTT() && next_b_formula != RawTT())
					RawLTLSupervisor(a_test || b_test, RawDisj(next_a_formula, next_b_formula))

				else if (next_b_formula != RawTT())
					RawLTLSupervisor(a_test || b_test, next_b_formula)

				else if (next_a_formula != RawTT())
					RawLTLSupervisor(a_test || b_test, next_a_formula)

				else
					RawLTLSupervisor(a_test || b_test, RawTT())

			case RawNeg(RawDisj(a, b)) => compute_next(state,RawConj(RawNeg(a), RawNeg(b)))

			case RawNext(f) =>
				RawLTLSupervisor(true, f)

			case RawNeg(RawNext(f)) => compute_next(state,RawNext(RawNeg(f)))

			case RawUntil(a, b) =>
				val next_a = compute_next(state,a)
				val next_b = compute_next(state,b)
				val a_test = next_a.success
				val next_a_formula = next_a.next_ltl
				val b_test = next_b.success
				val next_b_formula = next_b.next_ltl

				if (b_test)
					RawLTLSupervisor(true, RawTT())
				else if (a_test)
					RawLTLSupervisor(true, RawUntil(a, b))
				else
					RawLTLSupervisor(false, RawFF())

			case RawNeg(RawUntil(a, b)) => compute_next(state, RawRelease(RawNeg(a),RawNeg(b)))

			case RawRelease(a, b) =>
				val next_a = compute_next(state,a)
				val next_b = compute_next(state,b)
				val a_test = next_a.success
				val next_a_formula = next_a.next_ltl
				val b_test = next_b.success
				val next_b_formula = next_b.next_ltl

				if (b_test) {
					if (a_test)
						RawLTLSupervisor(true, RawTT())
					else
						RawLTLSupervisor(true, RawRelease(a, b))
				} else
					RawLTLSupervisor(false,RawFF())

			case RawNeg(RawRelease(a, b)) => compute_next(state,RawNext(RawUntil(RawNeg(a),RawNeg(b))))

			case RawFinally(f) => compute_next(state,RawUntil(RawTT(),f))
			case RawNeg(RawFinally(f)) => compute_next(state,RawNeg(RawUntil(RawTT(),f)))

			case RawGlobally(f) => compute_next(state,RawNeg(RawFinally(RawNeg(f))))
			case RawNeg(RawGlobally(f)) => compute_next(state,RawFinally(RawNeg(f)))

			case RawNeg(RawNeg(f)) => compute_next(state,f.asInstanceOf[RawLTL])

			case _ => RawLTLSupervisor(false,RawFF())

		}

	}

}

object RawGoalModelSupervisor {

	def factory(init:RawState, goals: Array[RawLTL]) : RawGoalModelSupervisor= {
		val zero: Array[RawLTLSupervisor] = for (g<-goals) yield RawLTLSupervisor(true,g)
		RawGoalModelSupervisor(zero).getNext(init)
	}
}


case class RawAction(id:String,pre:RawPredicate,effects:Array[RawEvolution])
case class RawEvolution(name : String, probability : Float, evo : Array[RawEvoOperator])

abstract class RawEvoOperator
case class RawAdd(add : RawVar) extends RawEvoOperator
case class RawRem(rmv : RawVar) extends RawEvoOperator



