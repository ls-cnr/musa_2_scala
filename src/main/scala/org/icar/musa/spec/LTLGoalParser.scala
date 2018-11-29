package org.icar.musa.spec

import org.icar.fol._

import scala.collection.mutable.ArrayBuffer
import scala.util.parsing.combinator.JavaTokenParsers

class LTLGoalParser extends JavaTokenParsers {




  def predicate : Parser[Predicate] = ident~"("~opt(term_list)~")" ^^ {
    case func~p_open~terms~p_close => { if (terms.isDefined) Predicate(func,terms.get.to[ArrayBuffer]) else Predicate(func,ArrayBuffer[Term]())  }
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
