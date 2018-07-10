package org.icar.musa.scenarios.sps

import scala.collection.mutable.ArrayBuffer

class Mission {
  /* order indicates priority */
  var vitals = ArrayBuffer[String]()
  var semivitals = ArrayBuffer[String]()
  var nonvitals = ArrayBuffer[String]()

  var vital_pow : Float = 5
  var semivital_pow : Float = 10
  var nonvital_pow : Float = 5

  var gen_pow = Map[String,Float]()

}

object Mission {
  def circuit3_mission_1 : Mission = {
    val m = new Mission()

    m.vitals =ArrayBuffer[String]("l2","l6","l9","l12","l16","l19","l22")
    m.semivitals =ArrayBuffer[String]("l3","l7","l13","l17","l23")
    m.nonvitals =ArrayBuffer[String]("l1","l4","l5","l8","l11","l14","l15","l18","l21","l24")

    m.gen_pow += ("mg1"->60)
    m.gen_pow += ("mg2"->60)
    m.gen_pow += ("aux1"->20)
    m.gen_pow += ("aux2"->20)

    m
  }
}
