package org.icar.musa.scenarios.sps

import org.icar.musa.context.{Measurables, StateOfWorld}
import org.icar.musa.main_entity._

import scala.collection.mutable.ArrayBuffer

class SPSConcreteRepository(circuit : Circuit, scenario : ReconfigurationScenario, repository:Array[AbstractCapability]) extends ConcreteRepository {

  override def load_concrete_capabilty: Array[ConcreteCapabilityFactory] = {
    var cap_list = ArrayBuffer[ConcreteCapabilityFactory]()

    for (gen <- circuit.generators if !scenario.generator_malfunctioning.contains(gen)) {
      val opt_c_on = generate_concrete_switch_on_generator(gen.id,repository)
      if (opt_c_on.isDefined)
        cap_list += opt_c_on.get

      val opt_c_off = generate_concrete_switch_off_generator(gen.id,repository)
      if (opt_c_off.isDefined)
        cap_list += opt_c_off.get
    }

    for (sw <- circuit.switcher if !scenario.switcher_malfunctioning.contains(sw)) {
      if (circuit.sw_map.contains(sw.id)) {
        val g2_name = circuit.sw_map(sw.id)
        val opt_comb = generate_concrete_combinated_on_off_switcher(sw.id,g2_name,repository)
        if (opt_comb.isDefined)
          cap_list += opt_comb.get
      } else {

        val parts = sw.id.split("switch")
        val second = parts(1)
        if (!second.startsWith("f")) {
          val opt_c_on = generate_concrete_close_switcher(sw.id,repository)
          if (opt_c_on.isDefined)
            cap_list += opt_c_on.get

          val opt_c_off = generate_concrete_open_switcher(sw.id,repository)
          if (opt_c_off.isDefined)
            cap_list += opt_c_off.get
        }
      }
    }

    cap_list.toArray
  }


  def generate_concrete_switch_on_generator(name : String, repository: Array[AbstractCapability]): Option[ConcreteCapabilityFactory] = {
    val abs_name = "switch_ON_"+name
    val abs = recover_abstract(abs_name,repository)
    if (abs.isDefined)
      Some(new SwitchOperationFactory(name,SW_ON(),abs.get))
    else
      None
  }

  def generate_concrete_switch_off_generator(name : String, repository: Array[AbstractCapability]): Option[ConcreteCapabilityFactory] = {
    val abs_name = "switch_OFF_"+name
    val abs = recover_abstract(abs_name,repository)
    if (abs.isDefined)
      Some(new SwitchOperationFactory(name,SW_OFF(),abs.get))
    else
      None
  }

  def generate_concrete_combinated_on_off_switcher(name1 : String, name2 : String, repository: Array[AbstractCapability]): Option[ConcreteCapabilityFactory] = {
    val abs_name = "CLOSE_"+name1+"_&_OPEN_"+name2
    val abs = recover_abstract(abs_name,repository)
    if (abs.isDefined)
      Some(new SwitchOperationFactory(name1+"_"+name2,SW_ON_OFF(),abs.get))
    else
      None
  }

  def generate_concrete_close_switcher(name : String, repository: Array[AbstractCapability]): Option[ConcreteCapabilityFactory] = {
    val abs_name = "CLOSE_"+name
    val abs = recover_abstract(abs_name,repository)
    if (abs.isDefined)
      Some(new SwitchOperationFactory(name,SW_CLOSE(),abs.get))
    else
      None
  }

  def generate_concrete_open_switcher(name : String, repository: Array[AbstractCapability]): Option[ConcreteCapabilityFactory] = {
    val abs_name = "OPEN_"+name
    val abs = recover_abstract(abs_name,repository)
    if (abs.isDefined)
      Some(new SwitchOperationFactory(name,SW_OPEN(),abs.get))
    else
      None
  }

}

class SwitchOperationFactory(val elem_name: String , val swtype : SWType,abs_cap : GroundedAbstractCapability) extends ConcreteCapabilityFactory {
  override def getAbstractName: String = abs_cap.name
  override def getInstance: ConcreteCapability = new SwitchOperation(elem_name,swtype,abs_cap)
}

abstract class SWType
case class SW_ON() extends SWType { override def toString="ON"}
case class SW_OFF() extends SWType { override def toString="OFF"}
case class SW_ON_OFF() extends SWType { override def toString="ON_OFF"}
case class SW_CLOSE() extends SWType { override def toString="CLOSE"}
case class SW_OPEN() extends SWType { override def toString="OPEN"}

class SwitchOperation(val elem_name: String , val swtype : SWType,abs_cap : GroundedAbstractCapability) extends ConcreteCapability("concrete_"+swtype+"_"+elem_name,abs_cap) {
  override def init: Unit = {}

  override def pre_start: Unit = {}

  override def execute(w : StateOfWorld,in:Measurables): Unit = {
    swtype match {
      case SW_ON() =>
        println("Open Switch "+elem_name)
      case SW_OFF() =>
        println("Close Switch "+elem_name)
      case SW_ON_OFF() =>
        println("Open and Close Switch "+elem_name)
      case SW_CLOSE() =>
        println("Close Switch "+elem_name)
      case SW_OPEN() =>
        println("Open Switch "+elem_name)
    }

    //println("executed "+name);
    set_scenario("1")

  }

  override def post_end: Unit = {}

  override def compensate: Unit = {}

  override def terminate: Unit = {}
}
