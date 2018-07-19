package org.icar.fol
import net.sf.tweety.lp.asp.parser.ASPParser
import net.sf.tweety.lp.asp.syntax.DLPHead

import scala.collection.mutable.ArrayBuffer

case class FOLCondition(formula:folFormula)

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
  def rule_for_asl : DLPHead = new DLPHead (ASPParser.parseRule(toString + ".").getConclusion.get(0))
}


object GroundPredicate {
  def apply(functional: String, terms: ConstantTerm*) : GroundPredicate = {
    var arr = ArrayBuffer[ConstantTerm]()
    for (t <- terms)
      arr +=  t

    GroundPredicate(functional,arr)
  }
}


sealed abstract class folFormula

/*
 * formula = literal | negative_literal | conjunction | disjunction
 */
case class GroundLiteral(predicate : GroundPredicate) extends folFormula
case class Literal(predicate : Predicate) extends folFormula

case class ExistQuantifier(predicate : Predicate, vars: ArrayBuffer[VariableTerm])
case class UnivQuantifier(predicate : Predicate, vars : ArrayBuffer[VariableTerm])
case class Negation(formula : folFormula) extends folFormula
case class Conjunction(formulas : ArrayBuffer[folFormula]) extends folFormula
case class Disjunction(formulas : ArrayBuffer[folFormula]) extends folFormula

object Conjunction {
  def apply(formulas: folFormula*): Conjunction = {
    var arr = ArrayBuffer[folFormula]()
    for (t <- formulas)
      arr +=  t

    Conjunction(arr)
  }
}
object Disjunction {
  def apply(formulas: folFormula*): Disjunction = {
    var arr = ArrayBuffer[folFormula]()
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



