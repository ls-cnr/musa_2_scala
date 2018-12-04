package org.icar.musa.spec

import org.icar.musa.context.{EvoOperator, Measurables, StateOfWorld}
import org.icar.musa.scenarios.WakeUpScenario

abstract class ConcreteRepository() {
  lazy val repository = load_concrete_capabilty

  def load_concrete_capabilty : Array[ConcreteCapabilityFactory]

  def recover_abstract(str: String, repository: Array[AbstractCapability]): Option[GroundedAbstractCapability] = {
    var cap : Option[GroundedAbstractCapability] = None

    for (c <- repository if c.name==str)
      cap = Some(c.asInstanceOf[GroundedAbstractCapability])

    cap
  }



}

abstract class ConcreteCapabilityFactory {
  def getAbstractName : String
  def getInstance : ConcreteCapability
}


abstract class ConcreteCapability(val name : String, val abs_cap : GroundedAbstractCapability) {
  var scn : String = ""
  var output = Measurables.empty


  def init : Unit
  def pre_start : Unit
  def execute(datain : Measurables) : Unit
  def post_end : Unit
  def compensate : Unit
  def terminate : Unit



  def set_scenario(str:String) : Unit = {scn = str}
  def set_data_out(m:Measurables) : Unit = {output = m}


  def get_simulated_scenario : Option[EvolutionScenario] = {
    if (abs_cap.scenarios.size==1) {
      val name = abs_cap.scenarios.keys.toList(0)
      Some(abs_cap.scenarios(name))
    } else if (abs_cap.scenarios.contains(scn))
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
