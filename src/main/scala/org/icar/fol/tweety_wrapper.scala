package org.icar.fol

import java.util
import net.sf.tweety.logics.fol.syntax.{ FolFormula => TweetyFOLFormula}
import net.sf.tweety.logics.commons.syntax.{Variable => TweetyVariable}


object TweetyArgument {
  import net.sf.tweety.logics.commons.syntax.interfaces.{Term => TweetyTTerm}
  import net.sf.tweety.logics.commons.syntax.{NumberTerm => TweetyNumberTerm}
  import net.sf.tweety.logics.commons.syntax.{Constant => TweetyConstant}

  def from(t : ConstantTerm) : TweetyTTerm[_] = {
    t match {
      case term: NumeralTerm => fromNumeral(term)
      case term: AtomTerm => fromAtom(term)
      case term: ConstantStringTerm => fromConstantString(term)
      case _ => null
    }
  }
  def fromConstantString(t : ConstantStringTerm) = new TweetyConstant(t.toString)
  def fromNumeral(t : NumeralTerm) = new TweetyNumberTerm(t.toString)
  def fromAtom(t : AtomTerm) = new TweetyConstant(t.atom)
  def fromVariable(t : VariableTerm) = new TweetyVariable(t.name)

}

object TweetyFormula {
  import net.sf.tweety.logics.commons.syntax.{Predicate => TweetyPredicate}
  import net.sf.tweety.logics.fol.syntax.{FOLAtom => TweetyAtom}
  import net.sf.tweety.logics.fol.syntax.{Disjunction => TweetyDisjunction}
  import net.sf.tweety.logics.fol.syntax.{Conjunction => TweetyConjunction}
  import net.sf.tweety.logics.fol.syntax.{Negation => TweetyNegation}
  import net.sf.tweety.logics.fol.syntax.{ExistsQuantifiedFormula => TweetyExist}
  import net.sf.tweety.logics.fol.syntax.{ForallQuantifiedFormula => TweetyUniv}


  def fromGround(p : GroundPredicate) : TweetyFOLFormula = {
    val pred = new TweetyPredicate(p.functional,p.terms.length)
    val a = new TweetyAtom( pred )
    for (t <- p.terms)
      a.addArgument(TweetyArgument.from(t))

    a
  }
  def fromGroundPred(p : GroundPredicate) : TweetyFOLFormula = {
    import net.sf.tweety.logics.commons.syntax.interfaces.{Term => TweetyTTerm}
    val pred = new TweetyPredicate(p.functional,p.terms.length)
    val args = new util.LinkedList[TweetyTTerm[_]]()

    for (t <- p.terms) {
      t match {
        case term: ConstantTerm => args.add(TweetyArgument.from(term))
        case _ =>
      }
    }
    new TweetyAtom( pred, args )
  }
  def fromPred(p : Predicate) : TweetyFOLFormula = {
    import net.sf.tweety.logics.commons.syntax.interfaces.{Term => TweetyTTerm}
    val pred = new TweetyPredicate(p.functional,p.terms.length)
    val args = new util.LinkedList[TweetyTTerm[_]]()

    for (t <- p.terms) {
      t match {
        case term: ConstantTerm => args.add(TweetyArgument.from(term))
        case term: VariableTerm => args.add(TweetyArgument.fromVariable(term))
        case _ =>
      }
    }
    new TweetyAtom( pred, args )
  }
  def fromFormula(p : HL_PredicateFormula) : TweetyFOLFormula = {
     p match {
       case disjunction: Disjunction => fromDisj(disjunction)
       case conjunction: Conjunction => fromConj(conjunction)
       case negation: Negation => fromNeg(negation)
       case literal: Literal => fromLit(literal)
       case literal: GroundLiteral => fromGroundLit(literal)
       case _ => null
     } 
  }
  def fromCond(p : FOLCondition) : TweetyFOLFormula = fromFormula(p.formula)
  def fromGroundLit(p : GroundLiteral) : TweetyFOLFormula = fromGroundPred(p.predicate)
  def fromLit(p : Literal) : TweetyFOLFormula = fromPred(p.predicate)
  def fromExist(q : ExistQuantifier): TweetyFOLFormula = {
    val v = new java.util.HashSet[TweetyVariable]()
    for (a <- q.vars)
      v.add(TweetyArgument.fromVariable(a))
    new TweetyExist(fromPred(q.predicate),v)
  }
  def fromUniv(q : UnivQuantifier): TweetyFOLFormula = {
    val v = new java.util.HashSet[TweetyVariable]()
    for (a <- q.vars)
      v.add(TweetyArgument.fromVariable(a))
    new TweetyUniv(fromPred(q.predicate),v)
  }
  def fromDisj(p : Disjunction) : TweetyFOLFormula = {
    val pred = new TweetyDisjunction()
    for (f <- p.formulas)
      pred.add(fromFormula(f))//.asInstanceOf[net.sf.tweety.logics.commons.syntax.RelationalFormula])
    pred
  }
  def fromConj(p : Conjunction) : TweetyFOLFormula = {
    val pred = new TweetyConjunction()
    for (f <- p.formulas)
      pred.add(fromFormula(f))//.asInstanceOf[net.sf.tweety.logics.commons.syntax.RelationalFormula])
    pred
  }
  def fromNeg(p : Negation) : TweetyFOLFormula = new TweetyNegation(fromFormula(p.formula))

}