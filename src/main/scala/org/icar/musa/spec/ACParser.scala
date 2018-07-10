package org.icar.musa.spec

import org.icar.fol._
import org.icar.musa.context.{AddEvoOperator, EvoOperator, RemoveAllEvoOperator, RemoveEvoOperator}

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

class ACParser extends FolParser {

  def cap_specification : Parser[List[AbstractCapability]] = rep(capability)

  def capability : Parser[AbstractCapability] = "capability"~>ident~"{"~pre~post~scenarios~"}" ^^ { case name~lp~pre~post~scen~rp => GroundedAbstractCapability(name,pre,post,scen)}

  //def capability_body : Parser[Any] = pre~post~scenarios

  def pre : Parser[FOLCondition] = "pre:"~>formula ^^ {case c => FOLCondition(c)}
  def post : Parser[FOLCondition] = "post:"~>formula ^^ {case c => FOLCondition(c)}

  def scenarios : Parser[Map[String,EvolutionScenario]] = rep("scenario"~>ident~scenario_body) ^^ {
    case list =>
      var m = Map[String,EvolutionScenario]()
      for (scen <- list) {
        m += (scen._1 -> scen._2)
      }
      m
  }

  def scenario_body : Parser[EvolutionScenario] = "["~>repsep(evolution_operator,",")<~"]" ^^ { case evo_list => EvolutionScenario(evo_list.toArray)}

  def evolution_operator: Parser[EvoOperator] = "add" ~> predicate ^^ {
    case p =>
      val l = p.asInstanceOf[Literal]
      var terms = ArrayBuffer[ConstantTerm]()
      for (t <- l.predicate.terms)
        terms += t.asInstanceOf[ConstantTerm]

      val g = GroundPredicate(l.predicate.functional, terms)
      AddEvoOperator(g)
  } |
    "remove" ~> predicate ^^ {
      case p =>
        val l = p.asInstanceOf[Literal]
        var terms = ArrayBuffer[ConstantTerm]()
        for (t <- l.predicate.terms)
          terms += t.asInstanceOf[ConstantTerm]

        val g = GroundPredicate(l.predicate.functional, terms)
        RemoveEvoOperator(g)
    } |
    "remove all" ~> ident ^^ { case s => RemoveAllEvoOperator(s) }
}

object TestLTLParser extends ACParser {
  def main(args : Array[String]) = {

    val file = "/Users/luca/workspace-scala/musa_2/src/test/scala/org/icar/musa/scenarios/PRIN_capabilities"
    val s = Source.fromFile(file)
    println(parseAll(cap_specification,s.mkString))


    /*println(parseAll(capability,"capability check_wakeup {" +
      "pre: sleeping(user) " +
      "post: wakeup(user) " +
      "scenario wakeup [ remove sleeping(user), add wakeup(user) ]" +
      "}"))*/
  }
}
