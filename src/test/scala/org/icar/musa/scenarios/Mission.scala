package org.icar.musa.scenarios

import scala.collection.mutable.ArrayBuffer

class Mission {
  /* order indicates priority */
  val vitals = ArrayBuffer[Load]
  val semivitals = ArrayBuffer[Load]
  val nonvitals = ArrayBuffer[Load]

  val vital_pow : Float = 5
  val semivital_pow : Float = 10
  val nonvital_pow : Float = 5

  val gen_pow = Map[Generator,Float]()

}
