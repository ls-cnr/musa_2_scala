package org.icar.musa.scenarios

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
}