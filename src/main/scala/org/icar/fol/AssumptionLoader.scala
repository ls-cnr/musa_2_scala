package org.icar.fol

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object AssumptionLoader {

  def load_from_file(file_name:String): AssumptionSet = {
    val list = ArrayBuffer[Assumption]()

    val source = Source.fromFile(file_name)
    for (line <- source.getLines())
      if (line.trim.length>0)
        list += Assumption(line)

    source.close()

    AssumptionSet(list: _*)
  }


}
