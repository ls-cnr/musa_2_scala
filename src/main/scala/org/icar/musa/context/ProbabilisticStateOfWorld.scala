package org.icar.musa.context

import org.icar.fol.GroundPredicate

import scala.collection.mutable.ArrayBuffer

class ProbabilisticStateOfWorld(var state : Map[GroundPredicate,Float]) {

  def classic_state : StateOfWorld = {
    var statements = new ArrayBuffer[GroundPredicate]

    for (s <- state.keySet)
      statements += s

    StateOfWorld(statements)
  }



}
