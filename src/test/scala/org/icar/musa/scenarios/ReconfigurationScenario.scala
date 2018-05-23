package org.icar.musa.scenarios

import scala.collection.mutable.ArrayBuffer

class ReconfigurationScenario {
  val switcher_state = Map[Switcher,Boolean]()
  val failures = ArrayBuffer[Connection]()
  val generator_malfunctioning = new ArrayBuffer[Generator]()


}
