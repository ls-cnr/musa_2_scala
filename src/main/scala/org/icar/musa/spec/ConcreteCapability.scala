package org.icar.musa.spec

import org.icar.musa.context.{EvoOperator, StateOfWorld}

abstract class ConcreteCapability(val name : String, val abs_cap : GroundedAbstractCapability) {
  var scn : String = ""


  def init : Unit
  def pre_start : Unit
  def execute : Unit
  def post_end : Unit
  def terminate : Unit



  def set_scenario(str:String) : Unit = {scn = str}

  def get_simulated_scenario : Option[EvolutionScenario] = {
    if (abs_cap.scenarios.contains(scn))
      abs_cap.scenarios.get(scn)
    else
      None
  }

  def apply_simulated_scenario(w : StateOfWorld) : StateOfWorld = {
    var ret_w = w

    if (abs_cap.scenarios.contains(scn)) {
      val evo_scn = abs_cap.scenarios.get(scn)
      if (evo_scn.isDefined) {
        val ops: Array[EvoOperator] = evo_scn.get.evo
        ret_w = StateOfWorld.extend(w,ops)
      }
    }

    ret_w
  }



}
