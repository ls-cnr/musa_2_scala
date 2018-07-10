package org.icar.fol

import scala.collection.mutable.ArrayBuffer
import scala.util.parsing.combinator.JavaTokenParsers

class FolParser extends JavaTokenParsers {

  def formula : Parser[folFormula] = predicate ||| conjunction ||| disjunction ||| negation

  def conjunction : Parser[folFormula] = "("~>repsep(formula, "and")<~")" ^^ { case arglist => Conjunction( arglist.to[ArrayBuffer]) }//("~formula~"and"~repsep(formula, "and")~")"
  def disjunction : Parser[folFormula] = "("~>repsep(formula, "or")<~")" ^^ { case arglist => Disjunction( arglist.to[ArrayBuffer]) }//"("~formula~"or"~repsep(formula, "or")~")"
  def negation : Parser[folFormula] = "not" ~> formula ^^ { case fol => Negation(fol)}

  def predicate : Parser[folFormula] = ident~"("~opt(term_list)~")" ^^ {
    case func~p_open~terms~p_close => { if (terms.isDefined) Literal(Predicate(func,terms.get.to[ArrayBuffer])) else Literal(Predicate(func,ArrayBuffer[Term]()))  }
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

object TestFolParser extends FolParser {
  def main(args : Array[String]) = {
    println(parseAll(formula,"(ready(document) and (evailable(document) or unav(doc)))"))
  }
}