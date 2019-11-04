package org.icar.fol

import scala.collection.mutable.ArrayBuffer

case class FOLCondition(formula:HighLevel_PredicateFormula)

case class Predicate private(functional:String, terms: ArrayBuffer[Term]= ArrayBuffer() ) {
  override def toString : String = functional+"("+term_list_string+")"
  def term_list_string : String = {
    var a_string: String = ""

    for (i <- terms.indices) {
      a_string += terms(i).toString
      if (i<terms.length-1)
      a_string += ","

    }
    a_string
  }

  def to_ground(assignments : Map[VariableTerm,String]):GroundPredicate = {
    val ground_terms = for (t<-terms) yield replace_var(t,assignments)
    GroundPredicate(functional,ground_terms)
  }


  private def replace_var(t: Term,assignments : Map[VariableTerm,String]):ConstantTerm = {
    t match {
      case AtomTerm(_) => t.asInstanceOf[AtomTerm]
      case StringTerm(_) => t.asInstanceOf[StringTerm]
      case NumeralTerm(_) => t.asInstanceOf[NumeralTerm]
      case IntegerTerm(_) => t.asInstanceOf[IntegerTerm]
      case TruthTerm() => TruthTerm()
      case FalsityTerm() => FalsityTerm()

      case VariableTerm(name) =>
          StringTerm(assignments(VariableTerm(name)))
      case _=> FalsityTerm()
    }
  }
}


object Predicate {
  def apply(functional: String, terms: Term*) : Predicate = {
    var arr = ArrayBuffer[Term]()
    for (t <- terms)
      arr +=  t

    Predicate(functional,arr)
  }
}


case class GroundPredicate private (functional:String, terms: ArrayBuffer[ConstantTerm] = ArrayBuffer() ) {
  def isTrue : Boolean = functional == "true"
  def isFalse : Boolean = functional == "false"

  override def toString : String = functional+"("+term_list_string+")"
  def term_list_string : String = {
    var a_string: String = ""

    for (i <- terms.indices) {
      if (terms(i).isInstanceOf[NumeralTerm]) {
        val n = terms(i).asInstanceOf[NumeralTerm]
        a_string += n.num.toInt
      } else
        a_string += terms(i).toString
      if (i<terms.length-1)
      a_string += ","

    }
    a_string
  }
}


object GroundPredicate {
  def apply(functional: String, terms: ConstantTerm*) : GroundPredicate = {
    var arr = ArrayBuffer[ConstantTerm]()
    for (t <- terms)
      arr +=  t

    GroundPredicate(functional,arr)
  }

  def apply(functional: String, terms: java.util.List[ConstantTerm]) : GroundPredicate = {
    var arr = ArrayBuffer[ConstantTerm]()
    val it = terms.iterator()
    while (it.hasNext)
      arr +=  it.next()

    GroundPredicate(functional,arr)
  }
}

/*
 * formula = literal | negative_literal | conjunction | disjunction
 */

sealed abstract class HighLevel_PredicateFormula
case class GroundLiteral(predicate : GroundPredicate) extends HighLevel_PredicateFormula
case class Literal(predicate : Predicate) extends HighLevel_PredicateFormula
case class ExistQuantifier(predicate : Predicate, vars: ArrayBuffer[VariableTerm]) extends HighLevel_PredicateFormula
case class UnivQuantifier(predicate : Predicate, vars : ArrayBuffer[VariableTerm]) extends HighLevel_PredicateFormula
case class Negation(formula : HighLevel_PredicateFormula) extends HighLevel_PredicateFormula
case class Conjunction(formulas : ArrayBuffer[HighLevel_PredicateFormula]) extends HighLevel_PredicateFormula
case class Disjunction(formulas : ArrayBuffer[HighLevel_PredicateFormula]) extends HighLevel_PredicateFormula
case class AlwaysTrue() extends HighLevel_PredicateFormula
case class AlwaysFalse() extends HighLevel_PredicateFormula

object Conjunction {
  def apply(formulas: HighLevel_PredicateFormula*): Conjunction = {
    var arr = ArrayBuffer[HighLevel_PredicateFormula]()
    for (t <- formulas)
      arr +=  t

    Conjunction(arr)
  }
}
object Disjunction {
  def apply(formulas: HighLevel_PredicateFormula*): Disjunction = {
    var arr = ArrayBuffer[HighLevel_PredicateFormula]()
    for (t <- formulas)
      arr +=  t

    Disjunction(arr)
  }
}

/* 
 * term = constant (atom, string or numeral) | variable 
 * */
sealed abstract class Term

case class VariableTerm(name : String) extends Term {
  override def toString : String = name
}

abstract class ConstantTerm extends Term
abstract class ConstantStringTerm extends ConstantTerm

case class AtomTerm(atom : String) extends ConstantTerm {
  override def toString : String = atom
}
case class NumeralTerm(num : Double) extends ConstantTerm {
  override def toString : String = num.toString
}
case class IntegerTerm(num : Int) extends ConstantTerm {
  override def toString : String = num.toString
}
case class TruthTerm() extends ConstantStringTerm {
  override def toString : String = "true"
}
case class FalsityTerm() extends ConstantStringTerm {
  override def toString : String = "false"
}
case class StringTerm(str : String) extends ConstantStringTerm {
  override def toString : String = "\""+str+"\""
}



