package org.icar.musa.scenarios.sps

import scala.collection.mutable.ArrayBuffer

class ReconfigurationScenario {
  var open_switchers = ArrayBuffer[String]()
  var up_generators = ArrayBuffer[String]()
  var failures = ArrayBuffer[String]()

  var generator_malfunctioning = new ArrayBuffer[String]()
  var switcher_malfunctioning = new ArrayBuffer[String]()

}

object ReconfigurationScenario {
  def scenario1 : ReconfigurationScenario = {
    val s = new ReconfigurationScenario()

    s.open_switchers = ArrayBuffer[String]("swp1","swaux1s","swp2","sws3","swp4","swaux2s","sws5","sws6","sws7")
    s.up_generators = ArrayBuffer[String]("mg1","mg2")

    s.failures = ArrayBuffer("c3_4","c4_5","c16_21")

    s
  }
  def scenario_circuit3_parsed_1 : ReconfigurationScenario = {
    val s = new ReconfigurationScenario()

    s.open_switchers = ArrayBuffer[String]("switchswp1","switchswaux1s","switchswp2","switchsws3","switchswp4","switchswaux2s","switchsws5","switchsws6","switchsws7","switchf1","switchf2","switchf3")//,"switchf4","switchf5")
    s.up_generators = ArrayBuffer[String]("mg1","mg2")

    //s.failures = ArrayBuffer("c3_4","c4_5","c16_21")

    s
  }
}