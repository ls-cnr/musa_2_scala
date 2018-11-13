package org.icar.musa.scenarios

import java.io.InputStream

object PRINWakeUp_APP extends App {

  val in = this.getClass.getClassLoader.getResource("./org/icar/musa/scenarios/PRIN_capabilities.cap")
  println("path "+in)


}
