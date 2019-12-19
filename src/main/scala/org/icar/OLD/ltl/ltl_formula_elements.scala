package org.icar.ltl

import org.icar.fol.{ConstantTerm, GroundPredicate}

import scala.collection.mutable.ArrayBuffer

sealed abstract class ltlFormula

sealed abstract class LTLOperator extends ltlFormula
case class Globally(formula : ltlFormula) extends LTLOperator
case class Finally(formula : ltlFormula) extends LTLOperator
case class Next(formula : ltlFormula) extends LTLOperator
case class Until(left : ltlFormula,right : ltlFormula) extends LTLOperator
case class Release(left : ltlFormula,right : ltlFormula) extends LTLOperator
case class LogicAtom(predicate : GroundPredicate) extends ltlFormula
case class LogicTrue() extends ltlFormula
case class LogicFalse() extends ltlFormula



abstract class LogicOperator extends ltlFormula
case class LogicImplication(left : ltlFormula,right : ltlFormula) extends LogicOperator
case class LogicBiImplication(left : ltlFormula,right : ltlFormula) extends LogicOperator
case class LogicNegation(formula : ltlFormula) extends LogicOperator
case class LogicConjunction(formulas : ArrayBuffer[ltlFormula]) extends LogicOperator
case class LogicDisjunction(formulas : ArrayBuffer[ltlFormula]) extends LogicOperator



object LogicAtom {
  def apply(functional: String, terms: ConstantTerm*) : LogicAtom = LogicAtom(GroundPredicate(functional,terms:_*))
}

object LogicConjunction {
  def apply(formulas: ltlFormula*): LogicConjunction = {
    val arr = ArrayBuffer[ltlFormula]()
    for (f <- formulas)
      arr += f
    LogicConjunction(arr)
  }


}

object LogicDisjunction {
  def apply(formulas: ltlFormula*): LogicDisjunction = {
    val arr = ArrayBuffer[ltlFormula]()
    for (f <- formulas)
      arr += f
    LogicDisjunction(arr)
  }
}