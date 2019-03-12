package org.icar.fol

import scala.collection.mutable.ArrayBuffer
import scala.util.parsing.combinator.JavaTokenParsers


class FormulaParser extends JavaTokenParsers {

  def formula : Parser[folFormula] = literal~bin_op~formula ^^ { case p~op~f =>
        op match {
          case "and" =>
            if (f.isInstanceOf[Conjunction]) {
              val c = f.asInstanceOf[Conjunction]
              var s = c.formulas
              s += p
              Conjunction(s)
            } else {
              Conjunction(p,f)
            }
          case "or" =>
            if (f.isInstanceOf[Disjunction]) {
              val c = f.asInstanceOf[Disjunction]
              var s = c.formulas
              s += p
              Disjunction(s)
            } else {
              Disjunction(p, f)
            }
        }} |
      "not" ~> "(" ~> formula <~ ")" ^^ {case f => Negation(f)} |
      literal ^^ {case p => p}

  def bin_op : Parser[Any] = "and" | "or" //| "->" | "<=>"

  def literal : Parser[folFormula] = predicate ^^ {case p => Literal(p)} | "not"~>predicate ^^ {case p => Negation(Literal(p))} | "(" ~> formula <~ ")" ^^ { case f => f }

  def predicate : Parser[Predicate] = ident~"("~opt(term_list)~")" ^^ {
    case func~p_open~terms~p_close => if (terms.isDefined) Predicate(func,terms.get.to[ArrayBuffer]) else Predicate(func,ArrayBuffer[Term]())
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


object TestFolParser extends FormulaParser {
  def main(args : Array[String]): Unit = {
    println(parseAll(formula,"not checked(user,entertainment) and not sleeping(user) and not ill(user) and ( entertainment_time(user) and not passed_entertainment_time(user) ) and location(user,living_room)"))
  }
}

