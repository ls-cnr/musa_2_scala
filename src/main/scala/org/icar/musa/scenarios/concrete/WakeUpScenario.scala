package org.icar.musa.scenarios.concrete

import akka.actor.{ActorRef, Props}
import org.icar.musa.actor.WorkerActor
import org.icar.musa.scenarios.PRINWakeUpScenario
import org.icar.musa.spec.{AbstractCapability, ConcreteCapability, ConcreteRepository, GroundedAbstractCapability}

import scala.collection.mutable.ArrayBuffer

class WakeUpConcreteRepository extends ConcreteRepository {

  def load_concrete_capabilty : Array[ConcreteCapability] = {
    var conc_repo : ArrayBuffer[ConcreteCapability] = ArrayBuffer()

    val repository = load_abs_capabilities

    val abstract1 = recover_abstract("check_wake_up",repository)
    if (abstract1.isDefined)
      conc_repo += new CheckWakeUp1(abstract1.get)


    val abstract2 = recover_abstract("remind_wake_up",repository)
    if (abstract2.isDefined)
      conc_repo += new RemindWakeUp1(abstract2.get)


    val abstract3 = recover_abstract("alert_anomaly",repository)
    if (abstract3.isDefined)
      conc_repo += new AlertAnomaly1(abstract3.get)

    conc_repo.toArray
  }

  def load_abs_capabilities : Array[AbstractCapability] = {
    val sc = new PRINWakeUpScenario //PRINEntertainmentScenario
    sc.capabilities
  }

}

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

