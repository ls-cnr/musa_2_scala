package org.icar.fol


import scala.collection.mutable.ArrayBuffer

case class FOLCondition(formula:HL_PredicateFormula)

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

  def isGround : Boolean = {
    var ground = true
    for (t<-terms if t.isInstanceOf[VariableTerm])
      ground = false
    ground
  }

  def get_grounded : Option[GroundPredicate] = {
    if (isGround) {
      var array : ArrayBuffer[ConstantTerm] = ArrayBuffer()
      for (t<-terms)
        t match {
          case AtomTerm(a) => array += AtomTerm(a)
          case StringTerm(s) => array += StringTerm(s)
          case NumeralTerm(n) => array += NumeralTerm(n)
          case IntegerTerm(i) => array += IntegerTerm(i)
          case TruthTerm() => array += TruthTerm()
          case FalsityTerm() => array += FalsityTerm()
          case _ => FalsityTerm()
        }
      Some(GroundPredicate(this.functional,array))

    } else {
      None
    }
  }

  def to_ground(assignments : Map[VariableTerm,ConstantTerm]):GroundPredicate = {
    val ground_terms = for (t<-terms) yield replace_var(t,assignments)
    GroundPredicate(functional,ground_terms)
  }


  private def replace_var(t: Term,assignments : Map[VariableTerm,ConstantTerm]):ConstantTerm = {
    t match {
      case AtomTerm(_) => t.asInstanceOf[AtomTerm]
      case StringTerm(_) => t.asInstanceOf[StringTerm]
      case NumeralTerm(_) => t.asInstanceOf[NumeralTerm]
      case IntegerTerm(_) => t.asInstanceOf[IntegerTerm]
      case TruthTerm() => TruthTerm()
      case FalsityTerm() => FalsityTerm()

      case VariableTerm(name) =>
          assignments(VariableTerm(name))
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

  def raw_description = {

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

sealed abstract class HL_PredicateFormula
case class GroundLiteral(predicate : GroundPredicate) extends HL_PredicateFormula
case class Literal(predicate : Predicate) extends HL_PredicateFormula
case class ExistQuantifier(predicate : Predicate, vars: ArrayBuffer[VariableTerm]) extends HL_PredicateFormula
case class UnivQuantifier(predicate : Predicate, vars : ArrayBuffer[VariableTerm]) extends HL_PredicateFormula
case class Negation(formula : HL_PredicateFormula) extends HL_PredicateFormula
case class Conjunction(formulas : ArrayBuffer[HL_PredicateFormula]) extends HL_PredicateFormula
case class Disjunction(formulas : ArrayBuffer[HL_PredicateFormula]) extends HL_PredicateFormula
case class AlwaysTrue() extends HL_PredicateFormula
case class AlwaysFalse() extends HL_PredicateFormula

object HL_PredicateFormula {
  def substitution(f:HL_PredicateFormula, assigned:Map[String,ConstantTerm]) : HL_PredicateFormula = {
    f match {
      case AlwaysTrue() => AlwaysTrue()
      case AlwaysFalse() => AlwaysFalse()
      case Disjunction(sf) =>
        val subterms = for (t<-sf) yield substitution(t,assigned)
        Disjunction(subterms)
      case Conjunction(sf) =>
        val subterms = for (t<-sf) yield substitution(t,assigned)
        Conjunction(subterms)
      case Negation(op) => Negation(substitution(op,assigned))
      case ExistQuantifier(predicate,_) => substitution(Literal(predicate),assigned)
      case UnivQuantifier(predicate,_) => substitution(Literal(predicate),assigned)
      case GroundLiteral(p) => GroundLiteral(p)
      case Literal(p) =>
        val p1 = substitution(p,assigned)
        val opt_p2 = p1.get_grounded
        if (opt_p2.isDefined)
          GroundLiteral(opt_p2.get)
        else
          Literal(p1)
      case _ =>    AlwaysTrue()
    }
  }
  def substitution(p:Predicate, assigned:Map[String,ConstantTerm]):Predicate = {
    var terms_array : ArrayBuffer[Term]=ArrayBuffer()
    for (t<-p.terms) {
      t match {
        case VariableTerm(name) =>
          if (assigned.contains(name))
            terms_array += assigned(name)
          else
            terms_array += VariableTerm(name)
        case AtomTerm(n) => terms_array += AtomTerm(n)
        case NumeralTerm(n) => terms_array += NumeralTerm(n)
        case StringTerm(s) => terms_array += StringTerm(s)
        case _ =>
      }
    }
    Predicate(p.functional,terms_array)
  }
}

object Conjunction {
  def apply(formulas: HL_PredicateFormula*): Conjunction = {
    var arr = ArrayBuffer[HL_PredicateFormula]()
    for (t <- formulas)
      arr +=  t

    Conjunction(arr)
  }
}
object Disjunction {
  def apply(formulas: HL_PredicateFormula*): Disjunction = {
    var arr = ArrayBuffer[HL_PredicateFormula]()
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



