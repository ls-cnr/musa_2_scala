package org.icar.musa.actor.concrete

import org.icar.musa.spec.{AbstractCapability, ConcreteCapability, GroundedAbstractCapability}

class CheckWakeUp1(abs_cap : GroundedAbstractCapability) extends ConcreteCapability("check_wakeup_1",abs_cap) {
  var count = 0 : Int

  override def init: Unit = { println("init CheckWakeUp"); count=0 }

  override def pre_start: Unit = {count += 1}

  override def execute: Unit = { println("executing CheckWakeUp") }

  override def post_end: Unit = { if (count==1) set_scenario("over_sleeping") else set_scenario("standing") }

  override def terminate: Unit = {}
}


class RemindWakeUp1(abs_cap : GroundedAbstractCapability) extends ConcreteCapability("remind_wakeup_1",abs_cap) {

  override def init: Unit = { println("init RemindWakeUp") }

  override def pre_start: Unit = { }

  override def execute: Unit = { println("executing RemindWakeUp") }

  override def post_end: Unit = { set_scenario("remind") }

  override def terminate: Unit = {}
}


class AlertAnomaly1(abs_cap : GroundedAbstractCapability) extends ConcreteCapability("alert_anomaly_1",abs_cap) {

  override def init: Unit = { println("init AlertAnomaly") }

  override def pre_start: Unit = { }

  override def execute: Unit = { println("executing AlertAnomaly") }

  override def post_end: Unit = { set_scenario("alert_cargiver") }

  override def terminate: Unit = {}
}

