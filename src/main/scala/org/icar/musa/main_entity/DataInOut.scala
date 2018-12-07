package org.icar.musa.main_entity

import scala.collection.mutable.ArrayBuffer

case class DataInSpecification(datas : ArrayBuffer[DataSpecification])

case class DataOutSpecification(datas : ArrayBuffer[DataSpecification])

case class DataSpecification(name : String, opt:Boolean=false)

