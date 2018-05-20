package org.icar.ltl

import org.icar.fol.{ConstantTerm, GroundPredicate, Predicate}

sealed abstract class ltlFormula

case class LogicAtom(predicate : GroundPredicate) extends ltlFormula

abstract class LTLOperator extends ltlFormula
case class Globally(formula : ltlFormula) extends LTLOperator
case class Finally(formula : ltlFormula) extends LTLOperator
case class Next(formula : ltlFormula) extends LTLOperator
case class Until(left : ltlFormula,right : ltlFormula) extends LTLOperator
case class Release(left : ltlFormula,right : ltlFormula) extends LTLOperator

abstract class LogicOperator extends ltlFormula
case class LogicImplication(left : ltlFormula,right : ltlFormula) extends LogicOperator
case class LogicBiImplication(left : ltlFormula,right : ltlFormula) extends LogicOperator
case class LogicNegation(formula : ltlFormula) extends LogicOperator
case class LogicConjunction(formulas : ltlFormula*) extends LogicOperator
case class LogicDisjunction(formulas : ltlFormula*) extends LogicOperator



object LogicAtom {
  def apply(functional: String, terms: ConstantTerm*) : LogicAtom = LogicAtom(GroundPredicate(functional,terms:_*))
}