package org.icar.musa.context

import org.icar.musa.main_entity.DataInSpecification
import scala.collection.mutable

class Measurables {
  private var map = mutable.Map[String,Any]()

  def hasVariable(name: String): Boolean = map.contains(name)
  def registerVariable(key:String, value:Any) : Boolean = {
    if (map.contains(key))
      false
    else {
      map.put(key , value)
      true
    }
  }
  def updateVariable(key:String, value:Any) : Boolean = {
    if (map.contains(key)) {
      map(key) = value
      true
    } else
      false
  }
  def getVariableValue(key:String) : Option[Any] = map.get(key)
  def getData(in: DataInSpecification): Option[Measurables] = {
    val m = Measurables.empty
    var error = false

    for (spec <- in.datas if error==false) {
      if (this.map.contains(spec.name))
        m.registerVariable(spec.name,map(spec.name))
      else {
        if (spec.opt==false)
          error = true
      }
    }

    if (error==false)
      Some(m)
    else
      None

  }

  def size : Int = map.size
  def variables: Iterable[String] = map.keys

  override def toString: String = {
    var string=""
    for (k <- map.keys)
      string +="v: "+k+" = "+map(k)

    string
  }
}


object Measurables {
  def empty : Measurables = new Measurables

  def subset(m : Measurables, sub : List[String]) : Measurables = {
    val result = new Measurables
    result.map = for (i <- m.map if sub.contains(i._1)) yield i
    result
  }

  def clone(m : Measurables) : Measurables = {
    val result = new Measurables
    result.map = for (i <- m.map) yield i
    result
  }
}


class DataIn extends Measurables
class DataOut extends Measurables

