package org.icar.musa.specification

import org.icar.fol.GroundPredicate
import org.icar.musa.context.{AddEvoOperator, EvoOperator, Measurables, RemoveEvoOperator}
import org.icar.musa.main_entity.{EvolutionScenario, LTLGoalParser}

import scala.concurrent.duration._

abstract class StateMonitorCapability {
  val envs : List[String] = List()
  private val ltlparser = new LTLGoalParser


  def name : String = {
    var n = ""
    for (e <- envs) n+=e+"_"
    n
  }

  def init() : Unit
  def check_state(in:Measurables) : EvolutionScenario
  def terminate() : Unit

  def delay : FiniteDuration = 1 second

  protected def remove(s:String): EvoOperator = {
    val parsed: ltlparser.ParseResult[GroundPredicate] = ltlparser.parseAll(ltlparser.predicate,s)

    if (parsed.successful)
      RemoveEvoOperator(parsed.get)
    else
      AddEvoOperator( ltlparser.parseAll(ltlparser.predicate,"error_in(parser)").get)
  }

  protected def add(s:String): EvoOperator = {
    val parsed: ltlparser.ParseResult[GroundPredicate] = ltlparser.parseAll(ltlparser.predicate,s)

    if (parsed.successful)
      AddEvoOperator(parsed.get)
    else
      AddEvoOperator( ltlparser.parseAll(ltlparser.predicate,"error_in(parser)").get)
  }

}
