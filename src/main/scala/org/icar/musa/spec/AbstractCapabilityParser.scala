package org.icar.musa.spec

import org.icar.fol._
import org.icar.musa.context.{AddEvoOperator, EvoOperator, RemoveAllEvoOperator, RemoveEvoOperator}

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

class AbstractCapabilityParser extends FormulaParser {

  def cap_specification : Parser[List[AbstractCapability]] = rep(capability)

  def capability : Parser[AbstractCapability] = "capability"~>ident~"{"~opt(in)~opt(out)~pre~post~scenarios~"}" ^^ {
    case name~_~in~out~pre~post~scen~_ =>
      val data_in = in.getOrElse(DataInSpecification(ArrayBuffer[DataSpecification]()))
      val data_out = out.getOrElse(DataOutSpecification(ArrayBuffer[DataSpecification]()))
      GroundedAbstractCapability(name,pre,post,scen,in = data_in,out = data_out)
    case _ =>
      GroundedAbstractCapability("error",FOLCondition(AlwaysTrue()),FOLCondition(AlwaysTrue()),Map())
  }

  //def capability_body : Parser[Any] = pre~post~scenarios
  def in : Parser[DataInSpecification] = "input:"~>data_list ^^ {
    list =>
      val buff = ArrayBuffer[DataSpecification]()
      for (item <- list) buff += item
      DataInSpecification(buff)
  }
  def out : Parser[DataOutSpecification] = "output:"~>data_list ^^ {
    list =>
      val buff = ArrayBuffer[DataSpecification]()
      for (item <- list) buff += item
      DataOutSpecification(buff)
  }

  def data_list : Parser[List[DataSpecification]] = repsep(data_description,",")
  def data_description : Parser[DataSpecification] = ident^^ {
    id => DataSpecification(id)
  } | ident<~"(opt)"^^ {
    id => DataSpecification(id,true)
  }

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
      //val l = p.asInstanceOf[Predicate]
      var terms = ArrayBuffer[ConstantTerm]()
      for (t <- p.terms)
        terms += t.asInstanceOf[ConstantTerm]

      val g = GroundPredicate(p.functional, terms)
      AddEvoOperator(g)
  } |
    "remove" ~> predicate ^^ {
      case p =>
        //val l = p.asInstanceOf[Predicate]
        var terms = ArrayBuffer[ConstantTerm]()
        for (t <- p.terms)
          terms += t.asInstanceOf[ConstantTerm]

        val g = GroundPredicate(p.functional, terms)
        RemoveEvoOperator(g)
    } |
    "remove all" ~> ident ^^ { case s => RemoveAllEvoOperator(s) }
}

object TestLTLParser extends AbstractCapabilityParser {
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
