package org.icar.musa.pmr

import scala.collection.mutable.ArrayBuffer

case class Sequence(seq: ArrayBuffer[String], loop: Boolean = false, exit: Boolean = false) {
  override def toString: String = {
    var string = "["
    for (s <- seq)
      string += s + ","
    string += "]"
    if (exit) string += "(exit)"
    if (loop) string += "(loop)"

    string
  }

  def contain_xor: Boolean = seq.exists( _.startsWith("X"))

  /*
    a sequence is safe IF
    a) terminates with "exit" or
    b) terminates with "loop" toward a node tha is before a X
  */
  def check_safeness: Boolean = {
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

      /* search, from the loop element, an X */
      var x = false
      while (x==false && i<seq.size) {
        if (seq(i).startsWith("X"))
          x=true
        i += 1
      }

      x

    } else if (exit)
      true
    else
      false
  }

}