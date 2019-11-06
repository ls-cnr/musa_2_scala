package org.icar.pmr_solver

import org.icar.fol.GroundPredicate


/******* LTL SUPERVISTOR STRATEGY ********/
class RawGoalModel(goals : Array[RawLTL]) {
  def getSupervisors(s:RawState) : Array[GoalSupervisor] = {
    for (g<-goals) yield new GoalSupervisor(s,g)
  }
}



class GoalSupervisor(state:RawState, ltl:RawLTL) {
  var success : Boolean = false
  private var next_ltl : RawLTL = RawTT()

  init

  def init : Unit = {
    val result = compute_next(ltl)
    success = result._1
    next_ltl = result._2
  }

  def isFullSatisfied : Boolean = {
    success==true && next_ltl==RawTT()
  }

  def getNextSupervisor(nextstate:RawState) : GoalSupervisor = new GoalSupervisor(nextstate,next_ltl)

  private def compute_next(formula : RawLTL) : (Boolean,RawLTL) = {

    formula match {
      case RawTT() => (true, RawTT())
      case RawNeg(RawTT()) => compute_next(RawFF())

      case RawFF() => (false, RawFF())
      case RawNeg(RawFF()) => compute_next(RawTT())

      case RawVar(i) =>
        if (state satisfies RawVar(i))
          (true, RawTT())
        else
          (false, RawFF())

      case RawNeg(RawVar(i)) =>
        if (state satisfies RawVar(i))
          (false, RawFF())
        else
          (true, RawTT())

      case RawConj(l, r) =>
        val a = l.asInstanceOf[RawLTL]
        val b = r.asInstanceOf[RawLTL]
        val (a_test, next_a_formula) = compute_next(a)
        val (b_test, next_b_formula) = compute_next(b)

        if (next_a_formula != RawTT() && next_b_formula != RawTT())
          (a_test && b_test, RawConj(next_a_formula, next_b_formula))

        else if (next_b_formula != RawTT())
          (a_test && b_test, next_b_formula)

        else if (next_a_formula != RawTT())
          (a_test && b_test, next_a_formula)

        else
          (a_test && b_test, RawTT())

      case RawNeg(RawConj(a, b)) => compute_next(RawDisj(RawNeg(a), RawNeg(b)))

      case RawDisj(l, r) =>
        val a = l.asInstanceOf[RawLTL]
        val b = r.asInstanceOf[RawLTL]
        val (a_test, next_a_formula) = compute_next(a)
        val (b_test, next_b_formula) = compute_next(b)

        if (next_a_formula != RawTT() && next_b_formula != RawTT())
          (a_test || b_test, RawDisj(next_a_formula, next_b_formula))

        else if (next_b_formula != RawTT())
          (a_test || b_test, next_b_formula)

        else if (next_a_formula != RawTT())
          (a_test || b_test, next_a_formula)

        else
          (a_test || b_test, RawTT())

      case RawNeg(RawDisj(a, b)) => compute_next(RawConj(RawNeg(a), RawNeg(b)))

      case RawNext(f) =>
        (true, f)

      case RawNeg(RawNext(f)) => compute_next(RawNext(RawNeg(f)))

      case RawUntil(a, b) =>
        val (a_test, next_a_formula) = compute_next(a)
        val (b_test, next_b_formula) = compute_next(b)

        if (b_test)
          (true, RawTT())
        else if (a_test)
          (true, RawUntil(a, b))
        else
          (false, RawFF())

      case RawNeg(RawUntil(a, b)) => compute_next(RawRelease(RawNeg(a),RawNeg(b)))

      case RawRelease(a, b) =>
        val (a_test, next_a_formula) = compute_next(a)
        val (b_test, next_b_formula) = compute_next(b)

        if (b_test) {
          if (a_test)
            (true, RawTT())
          else
            (true, RawRelease(a, b))
        } else
          (false,RawFF())

      case RawNeg(RawRelease(a, b)) => compute_next(RawNext(RawUntil(RawNeg(a),RawNeg(b))))

      case RawFinally(f) => compute_next(RawUntil(RawTT(),f))
      case RawNeg(RawFinally(f)) => compute_next(RawNeg(RawUntil(RawTT(),f)))

      case RawGlobally(f) => compute_next(RawNeg(RawFinally(RawNeg(f))))
      case RawNeg(RawGlobally(f)) => compute_next(RawFinally(RawNeg(f)))

      case RawNeg(RawNeg(f)) => compute_next(f.asInstanceOf[RawLTL])

      case _ => (false,RawFF())

    }

  }

}






