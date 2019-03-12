package org.icar.musa.scenarios.sps

import scala.collection.mutable.ArrayBuffer

class ReconfigurationScenario {
  var open_switchers: ArrayBuffer[String] = ArrayBuffer[String]()
  var up_generators: ArrayBuffer[String] = ArrayBuffer[String]()
  var failures: ArrayBuffer[String] = ArrayBuffer[String]()

  val generator_malfunctioning = new ArrayBuffer[String]()
  val switcher_malfunctioning = new ArrayBuffer[String]()

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

    s.open_switchers = ArrayBuffer[String]("switchswp1","switchswaux1s","switchswp2","switchsws3","switchswp4","switchswaux2s","switchsws5","switchsw15","switchsw18","switchsw21","switchsw24","switchsws6","switchsws7","switchswauxg1","switchswauxg2","switchsw10","switchsw20","switchf1","switchf2","switchf3")
    s.up_generators = ArrayBuffer[String]("mg1","mg2")

    //s.failures = ArrayBuffer("c3_4","c4_5","c16_21")

    s
  }
}