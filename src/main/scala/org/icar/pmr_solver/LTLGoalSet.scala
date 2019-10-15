package org.icar.pmr_solver

import net.sf.tweety.logics.fol.syntax.{FOLAtom, FolFormula}
import org.icar.fol.{AtomTerm, GroundPredicate, TweetyFormula}


/******* LTL SUPERVISTOR STRATEGY ********/

class GoalSupervisor(state:Node, ltl:LTLformula) {
  var success : Boolean = false
  private var next_ltl : LTLformula = Empty()

  init

  def init : Unit = {
    val result = compute_next(ltl)
    success = result._1
    next_ltl = result._2
  }

  def getNextSupervisor(nextstate:Node) : GoalSupervisor = new GoalSupervisor(nextstate,next_ltl)

  private def entail(p:FolFormula) : Boolean = state.interpretation satisfies p

  private def compute_next(formula : LTLformula) : (Boolean,LTLformula) = {
    //def next(formula : LTLformula, check : (Predicate)=>Boolean) : (Boolean,LTLformula) = {
    formula match {
      case Empty() => (true, Empty())
      case True() => (true, Empty())
      case Negation(True()) => compute_next(False())

      case False() => (false, Empty())
      case Negation(False()) => compute_next(True())

      case Predicate(p) =>
        //val a = new FOLAtom(new FOLPredicate(p))
        val test = entail(TweetyFormula.fromGround(p))
        if (test)
          (true, Empty())
        else
          (false, Empty())

      case Negation(Predicate(p)) =>
        val test = entail(TweetyFormula.fromGround(p))
        if (test)
          (false, Empty())
        else
          (true, Empty())

      case Conjunction(a, b) =>
        val (a_test, next_a_formula) = compute_next(a)
        val (b_test, next_b_formula) = compute_next(b)

        if (next_a_formula != Empty() && next_b_formula != Empty())
          (a_test && b_test, Conjunction(next_a_formula, next_b_formula))

        else if (next_a_formula == Empty())
          (a_test && b_test, next_b_formula)

        else if (next_b_formula == Empty())
          (a_test && b_test, next_a_formula)

        else
          (a_test && b_test, Empty())

      case Negation(Conjunction(a, b)) => compute_next(Disjunction(Negation(a), Negation(b)))

      case Disjunction(a, b) =>
        val (a_test, next_a_formula) = compute_next(a)
        val (b_test, next_b_formula) = compute_next(b)

        if (next_a_formula != Empty() && next_b_formula != Empty())
          (a_test || b_test, Conjunction(next_a_formula, next_b_formula))

        else if (next_a_formula == Empty())
          (a_test || b_test, next_b_formula)

        else if (next_b_formula == Empty())
          (a_test || b_test, next_a_formula)

        else
          (a_test || b_test, Empty())

      case Negation(Disjunction(a, b)) => compute_next(Conjunction(Negation(a), Negation(b)))

      case Next(f) =>
        (true, f)

      case Negation(Next(f)) => compute_next(Next(Negation(f)))

      case Until(a, b) =>
        val (a_test, next_a_formula) = compute_next(a)
        val (b_test, next_b_formula) = compute_next(b)

        if (b_test)
          (true, Empty())
        else if (a_test)
          (true, Until(a, b))
        else
          (false, Empty())

      case Negation(Until(a, b)) => compute_next(Release(Negation(a),Negation(b)))

      case Release(a, b) =>
        val (a_test, next_a_formula) = compute_next(a)
        val (b_test, next_b_formula) = compute_next(b)

        if (b_test) {
          if (a_test)
            (true, Empty())
          else
            (true, Release(a, b))
        } else
          (false,Empty())

      case Negation(Release(a, b)) => compute_next(Next(Until(Negation(a),Negation(b))))

      case Finally(f) => compute_next(Until(True(),f))
      case Negation(Finally(f)) => compute_next(Negation(Until(True(),f)))
      /*val (f_test,next_formula) = next(f,check)

      if (f_test)
        (true,Empty())
      else
        (true,Finally(f))*/

      case Globally(f) => compute_next(Negation(Finally(Negation(f))))
      case Negation(Globally(f)) => compute_next(Finally(Negation(f)))

      case Negation(Negation(f)) => compute_next(f)

      case _ => (false,Empty())

    }



  }

}






/******* LTL SYNTAX DEFINITION ********/


sealed abstract class LTLformula

case class Predicate(p:GroundPredicate) extends LTLformula
case class True() extends LTLformula
case class False() extends LTLformula
case class Empty() extends LTLformula

case class Implication(left : LTLformula,right : LTLformula) extends LTLformula
case class BiImplication(left : LTLformula,right : LTLformula) extends LTLformula
case class Negation(formula : LTLformula) extends LTLformula
case class Conjunction(left : LTLformula,right : LTLformula) extends LTLformula
case class Disjunction(left : LTLformula,right : LTLformula) extends LTLformula

case class Globally(formula : LTLformula) extends LTLformula
case class Finally(formula : LTLformula) extends LTLformula
case class Next(formula : LTLformula) extends LTLformula
case class Until(left : LTLformula,right : LTLformula) extends LTLformula
case class Release(left : LTLformula,right : LTLformula) extends LTLformula

