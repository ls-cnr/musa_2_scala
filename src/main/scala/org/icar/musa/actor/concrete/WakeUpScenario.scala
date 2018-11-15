package org.icar.musa.actor.concrete

import org.icar.musa.spec.{AbstractCapability, ConcreteCapability, GroundedAbstractCapability}

class CheckWakeUp1(abs_cap : GroundedAbstractCapability) extends ConcreteCapability("check_wakeup_1",abs_cap) {
  var count = 0 : Int

  override def init: Unit = { println("init CheckWakeUp"); count=0 }

  override def pre_start: Unit = { println("prestart CheckWakeUp"); count += 1}

  override def execute: Unit = { println("executing CheckWakeUp"); if (count<2) set_scenario("over_sleeping") else set_scenario("anomaly") }

  override def post_end: Unit = { println("CheckWakeUp has been successfull") }

  override def compensate: Unit = { println("compensate CheckWakeUp") }

  override def terminate: Unit = { println("delete CheckWakeUp") }
}


class RemindWakeUp1(abs_cap : GroundedAbstractCapability) extends ConcreteCapability("remind_wakeup_1",abs_cap) {

  override def init: Unit = { println("init RemindWakeUp") }

  override def pre_start: Unit = { println("prestart RemindWakeUp") }

  override def execute: Unit = { println("executing RemindWakeUp"); set_scenario("remind") }

  override def post_end: Unit = { println("RemindWakeUp has been successfull") }

  override def compensate: Unit = { println("compensate RemindWakeUp") }

  override def terminate: Unit = { println("delete RemindWakeUp") }
}


class AlertAnomaly1(abs_cap : GroundedAbstractCapability) extends ConcreteCapability("alert_anomaly_1",abs_cap) {

  override def init: Unit =       { println("init AlertAnomaly") }

  override def pre_start: Unit =  { println("prestart AlertAnomaly") }

  override def execute: Unit =    { println("executing AlertAnomaly"); set_scenario("alert_cargiver") }

  override def post_end: Unit =   { println("AlertAnomaly has been successfull") }

  override def compensate: Unit = { println("compensate AlertAnomaly") }

  override def terminate: Unit =  { println("delete AlertAnomaly") }
}

