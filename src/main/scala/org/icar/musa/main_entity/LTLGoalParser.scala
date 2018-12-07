package org.icar.musa.main_entity

import org.icar.fol._
import org.icar.ltl._

import scala.collection.mutable.ArrayBuffer
import scala.util.parsing.combinator.JavaTokenParsers

class LTLGoalParser extends JavaTokenParsers {

  def goal : Parser[LTLGoal] =
    "GOAL"~"{"~goal_spec~"}" ^^ {
      case "GOAL"~"{"~x~"}" => LTLGoal(LogicImplication(x._1,Finally(x._2)))
      case _ => println("error 1"); LTLGoal(LogicFalse())
    } |
      "GOAL"~"="~formula~"." ^^ {
        case "GOAL"~"="~f~"." => LTLGoal(f)
        case _ => println("error 2"); LTLGoal(LogicFalse())
      }

  def goal_spec : Parser[(ltlFormula,ltlFormula)] = pre~post ^^ {
    case x~y => (x,y)
    case _ => println("error 3"); (LogicFalse(),LogicFalse())
  }
  def pre : Parser[ltlFormula] = "when"~>folformula
  def post : Parser[ltlFormula] = "the system should"~>folformula

  /* classical LTL operators */
  def formula : Parser[ltlFormula] =
    conjunction~rep("or"~>conjunction) ^^ {
      case f~List() => f
      case f~elements =>
        val list = f :: elements
        LogicDisjunction(list.to[ArrayBuffer])
    }
  def conjunction : Parser[ltlFormula] = ltl_factor~rep("and"~>ltl_factor) ^^ {
    case f~List() => f
    case f~elements =>
      val list = f :: elements
      LogicConjunction(list.to[ArrayBuffer])
  }
  def ltl_factor : Parser[ltlFormula] =
    factor~opt(BINOP~factor) ^^ {
      case f~None => f
      case f1~Some("U"~f2) => Until(f1,f2)
      case f1~Some("R"~f2) => Release(f1,f2)
      case f1~Some("->"~f2) => LogicImplication(f1,f2)
      case f1~Some("<->"~f2) => LogicBiImplication(f1,f2)
      case f1~Some(op~f2) => println("error 4: "+op); LogicFalse()
    }
  def factor : Parser[ltlFormula] =
    "true" ^^ { x=>LogicTrue() } |
      "false"^^ { x=>LogicFalse() } |
      predicate^^{ p=>LogicAtom(p)} |
      "("~>formula<~")" |
      UNOP~formula ^^{
        case "G"~f => Globally(f)
        case "F"~f => Finally(f)
        case "X"~f => Next(f)
        case "not"~f => LogicNegation(f)
        case _ => println("error 5"); LogicFalse()
      }
  def BINOP : Parser[Any] = "U" | "R" | "->" | "<->"
  def UNOP : Parser[Any] = "G" | "F" | "X" | "not"


  /* no LTL operators */
  def folformula : Parser[ltlFormula] = folconjunction~rep("or"~>folconjunction)^^ {
    case f~List() => f
    case f~elements =>
      val list = f :: elements
      LogicDisjunction(list.to[ArrayBuffer])
  }
  def folconjunction : Parser[ltlFormula] = fol_factor~rep("and"~>fol_factor) ^^ {
    case f~List() => f
    case f~elements =>
      val list = f :: elements
      LogicConjunction(list.to[ArrayBuffer])
    case _ => println("error 6"); LogicFalse()
  }
  def fol_factor : Parser[ltlFormula] = pred_factor~opt(fol_BINOP~pred_factor)^^ {
    case f~None => f
    case f1~Some("->"~f2) => LogicImplication(f1,f2)
    case f1~Some("<->"~f2) => LogicBiImplication(f1,f2)
    case _ => println("error 7"); LogicFalse()
  }
  def pred_factor : Parser[ltlFormula] =
    predicate ^^ {
      p=>LogicAtom(p)
    } |
      "("~>folformula<~")" |
      fol_UNOP~folformula ^^ {
        case "not"~f => LogicNegation(f)
        case _ => println("error 8"); LogicFalse()
      }
  def fol_BINOP : Parser[Any] = "->" | "<->"
  def fol_UNOP : Parser[Any] =  "not"


  def predicate : Parser[GroundPredicate] = ident~"("~opt(term_list)~")" ^^ {
    case func~p_open~terms~p_close => {
      if (terms.isDefined)
        GroundPredicate(func,terms.get.to[ArrayBuffer])
      else
        GroundPredicate(func,ArrayBuffer[ConstantTerm]())  }
  }
  def term_list : Parser[List[ConstantTerm]] = repsep(term,",")
  def term  : Parser[ConstantTerm] = constant | atom
  def constant : Parser[ConstantTerm] =
    floatingPointNumber ^^ (x => NumeralTerm(x.toDouble)) |
      stringLiteral ^^(x => StringTerm(x))
  def atom : Parser[ConstantTerm] =
    ident ^^ (x=>AtomTerm(x)) |
      "true" ^^ (x=>TruthTerm()) |
      "false" ^^ (x=>FalsityTerm())

}


object TestFolParser extends LTLGoalParser {
  def main(args : Array[String]) = {
    println(
      parseAll(goal,"GOAL { when wake_up_time(user) the system should (standing(user) or alert_thrown(user) ) }"))
  }
}
// OK "G read(x) and (F worked(x) or F refused(z))"
// OK "letto(x) and not (read(x) U (F worked(y) or released(z) ))"
// OK "GOAL { when ready(x) the system should (produce(y) and confirm(s) ) }"
// OK "GOAL = G read(x) and (F worked(x) or F refused(z))."


/* ONLY GRAMMAR

  def goal : Parser[Any] =
    "GOAL"~"{"~goal_spec~"}"  |
    "GOAL"~"="~formula~"."

  def goal_spec : Parser[Any] = pre~post
  def pre : Parser[Any] = "when"~>folformula
  def post : Parser[Any] = "the system should"~>folformula

  /* classical LTL operators */
  def formula : Parser[Any] =
    conjunction~rep("or"~>conjunction)
  def conjunction : Parser[Any] = ltl_factor~rep("and"~>ltl_factor)
  def ltl_factor : Parser[Any] =
    factor~opt(BINOP~factor)
  def factor : Parser[Any] =
      "true" |
      "false" |
      predicate |
      "("~>formula<~")" |
      UNOP~formula
  def BINOP : Parser[Any] = "and" | "or" | "U" | "R" | "->" | "<->"
  def UNOP : Parser[Any] = "G" | "F" | "X" | "not"


  /* no LTL operators */
  def folformula : Parser[Any] = folconjunction~rep("or"~>folconjunction)
  def folconjunction : Parser[Any] = fol_factor~rep("and"~>fol_factor)
  def fol_factor : Parser[Any] = pred_factor~opt(fol_BINOP~pred_factor)
  def pred_factor : Parser[Any] =
    "true" |
      "false" |
      predicate |
      "("~>folformula<~")" |
      UNOP~folformula
  def fol_BINOP : Parser[Any] = "and" | "or" | "->" | "<->"
  def fol_UNOP : Parser[Any] =  "not"


  def predicate : Parser[Any] = ident~"("~opt(term_list)~")"
  def term_list : Parser[Any] = repsep(term,",")
  def term  : Parser[Any] = constant | atom
  def constant : Parser[Any] =
      floatingPointNumber  |
      stringLiteral
  def atom : Parser[Any] =
      ident |
      "true" |
      "false"


 */