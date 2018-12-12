package org.icar.musa.specification

import org.icar.musa.context.Measurables
import org.icar.musa.main_entity.EvolutionScenario

import scala.concurrent.duration._

abstract class StateMonitorCapability {
  val envs : List[String] = List()

  def name : String = {
    var n = ""
    for (e <- envs) n+=e+"_"
    n
  }

  def init : Unit
  def check_state(in:Measurables) : EvolutionScenario
  def terminate : Unit

  def delay : FiniteDuration = 1 second
}
