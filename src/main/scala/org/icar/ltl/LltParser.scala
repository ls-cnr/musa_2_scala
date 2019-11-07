package org.icar.ltl

import org.icar.fol._

import scala.collection.mutable.ArrayBuffer
import scala.util.parsing.combinator.JavaTokenParsers


class LltParser extends JavaTokenParsers {

  def ltlformula : Parser[ltlFormula] = ltlatom ||| ltltemporal ||| ltllogical

  def ltltemporal : Parser[ltlFormula] = ltlfinally ||| ltlglobally ||| ltlnext ||| ltluntil ||| ltlrelease
  def ltllogical : Parser[ltlFormula] = ltlconjunction ||| ltldisjunction ||| ltlnegation

  def ltlfinally : Parser[ltlFormula] = "F"~>ltlformula ^^ {case fol => Finally(fol)}
  def ltlglobally : Parser[ltlFormula] = "G"~>ltlformula ^^ {case fol => Globally(fol)}
  def ltlnext : Parser[ltlFormula] = "X"~>ltlformula ^^ {case fol => Next(fol)}

  def ltluntil : Parser[ltlFormula] = "("~>ltlformula~"U"~ltlformula<~")" ^^ {case fol1~op~fol2 => Until(fol1,fol2)}
  def ltlrelease : Parser[ltlFormula] = "("~>ltlformula~"R"~ltlformula<~")" ^^ {case fol1~op~fol2 => Release(fol1,fol2)}

  def ltlconjunction : Parser[ltlFormula] = "("~>repsep(ltlformula, "and")<~")" ^^ { case arglist => LogicConjunction( arglist.to[ArrayBuffer]) }
  def ltldisjunction : Parser[ltlFormula] = "("~>repsep(ltlformula, "or")<~")"  ^^ { case arglist => LogicDisjunction( arglist.to[ArrayBuffer]) }
  def ltlnegation : Parser[ltlFormula] = "not" ~> ltlformula ^^ { case fol => LogicNegation(fol)}
  def ltlimplication : Parser[ltlFormula] = "("~>ltlformula~"->"~ltlformula<~")" ^^ { case fol1~op~fol2 => LogicImplication(fol1,fol2)}
  def ltlbiimplication : Parser[ltlFormula] = "("~>ltlformula~"<->"~ltlformula<~")" ^^ { case fol1~op~fol2 => LogicBiImplication(fol1,fol2)}


  def ltlatom : Parser[ltlFormula] = predicate ^^ {
    case p => val l = p.asInstanceOf[Literal]
      var terms = ArrayBuffer[ConstantTerm]()
      for (t <- l.predicate.terms)
        terms += t.asInstanceOf[ConstantTerm]
      LogicAtom(GroundPredicate(l.predicate.functional,terms))
  }

  def predicate : Parser[HL_PredicateFormula] = ident~"("~opt(term_list)~")" ^^ {
    case func~p_open~terms~p_close => if (terms.isDefined) Literal(Predicate(func,terms.get.to[ArrayBuffer])) else Literal(Predicate(func,ArrayBuffer[Term]()))
  }


  def term_list : Parser[List[Term]] = repsep(term,",")

  def term  : Parser[Term] = constant | atom

  def constant : Parser[ConstantTerm] =
    floatingPointNumber ^^ (x => NumeralTerm(x.toDouble)) |
      stringLiteral ^^ (x => StringTerm(x))
  def atom : Parser[ConstantTerm] =
    ident ^^ (x=>AtomTerm(x)) |
      "true" ^^ (x=>TruthTerm()) |
      "false" ^^ (x=>FalsityTerm())

}



object TestLTLParser extends LltParser {
  def main(args : Array[String]): Unit = {
    println(parseAll(ltlformula,"(available(doc) U ready(doc))"))
  }
}