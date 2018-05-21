package org.icar.musa.pmr

import scala.collection.mutable.ArrayBuffer

case class StateSequence(seq: ArrayBuffer[String], loop: Boolean = false, exit: Boolean = false) {
  override def toString: String = {
    var string = "["
    for (s <- seq)
      string += s + ","
    string += "]"
    if (exit) string += "(exit)"
    if (loop) string += "(loop)"

    string
  }

  def contain_xor(decision_map : Map[(String, String), DecisionPoint]): Boolean = {
    var test = false

    for (i <- seq.indices if test==false) {
      if (i>0) {
        val start = seq(i-1)
        val end = seq(i)
        if (decision_map.contains((start,end)))
          test = true
      }
    }

    test
  }

  /*
    a sequence is safe IF
    a) terminates with "exit" or
    b) terminates with "loop" toward a node tha is before a X
  */
  def check_safeness(decision_map : Map[(String, String), DecisionPoint]): Boolean = {
    if (loop) {
      val loop_elem = seq.last
      var pos = -1

      /* search from start the loop element */
      var i = 0
      while (pos != -1 && i<seq.size) {
        if (seq(i)==loop_elem)
          pos = i
        i += 1
      }

      val tail = StateSequence(seq.drop(pos))
      val x = tail.contain_xor(decision_map)

      /*
      search, from the loop element, an X
      var x = false
      while (x==false && i<seq.size) {
        if (seq(i).startsWith("X"))
          x=true
        i += 1
      }
      */

      x

    } else if (exit)
      true
    else
      false
  }

}