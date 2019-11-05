package org.icar.musa.context
import scala.collection.mutable.ArrayBuffer
import org.icar.fol._

case class StateOfWorld private(statements : ArrayBuffer[GroundPredicate]) {
  
  override def toString: String = {
    var a_string: String = ""

    a_string += "["
    for (i <- statements.indices) {
      a_string += statements(i).toString
      if (i<statements.length-1)
      a_string += ","
        
    }
    a_string += "]"
    
    a_string
  }
  
  def stat_clone : Array[GroundPredicate] = statements.clone().toArray

}

object StateOfWorld {
  def empty : StateOfWorld = StateOfWorld(ArrayBuffer[GroundPredicate]())

  def apply (terms: java.util.List[GroundPredicate]) : StateOfWorld = {
    var arr = ArrayBuffer[GroundPredicate]()
    val it = terms.iterator()
    while (it.hasNext)
      arr +=  it.next()

    StateOfWorld(arr)
  }

  def create(statements : GroundPredicate*) : StateOfWorld = {

    val sorted : ArrayBuffer[GroundPredicate] = statements.to[ArrayBuffer].sortWith(_.toString < _.toString)
    StateOfWorld(sorted)
  }
  def create(statements : Array[GroundPredicate]) : StateOfWorld = {
    val sorted = statements.to[ArrayBuffer].sortWith(_.toString < _.toString)
    StateOfWorld(sorted)
  }
  
  def extend(w : StateOfWorld, new_statements : GroundPredicate*) : StateOfWorld = {
    val a = w.stat_clone.to[ArrayBuffer]
    for (x <- new_statements if !a.contains(x))
       a.append(x)
       
    StateOfWorld.create(a.toArray) 
  }

  def extend(w : StateOfWorld, new_w : StateOfWorld) : StateOfWorld = {
    val a = w.stat_clone.to[ArrayBuffer]
    for (x <- new_w.statements if !a.contains(x))
      a.append(x)

    StateOfWorld.create(a.toArray)
  }

  def extend(w : StateOfWorld, op : EvoOperator) : StateOfWorld = {
    var a = w.stat_clone.to[ArrayBuffer]
    op match {
      case Deprec_AddEvoOperator(add) => if (!a.contains(add)) a.append(add)
      case Deprec_RemoveEvoOperator(rmv) =>
        val i = a.indexOf(rmv)
        if (i>=0) a.remove(i)

      case Deprec_RemoveAllEvoOperator(rmv_all) =>
        val aa = ArrayBuffer[GroundPredicate]()
        
        for (x <- a) {
          if (x.functional!=rmv_all)
            aa.append(x) 
        }
        
        a = aa

      case _ =>
    }
    StateOfWorld.create(a.toArray) 
  }

  def extend(w : StateOfWorld, ops : Array[EvoOperator]) : StateOfWorld = {
    var a = w.stat_clone.to[ArrayBuffer]

    for (op <- ops)
      op match {
        case Deprec_AddEvoOperator(add) => if (!a.contains(add)) a.append(add)
        case Deprec_RemoveEvoOperator(rmv) =>
          val i = a.indexOf(rmv)
          if (i>=0) a.remove(i)

        case Deprec_RemoveAllEvoOperator(rmv_all) =>
          val aa = ArrayBuffer[GroundPredicate]()

          for (x <- a) {
            if (x.functional!=rmv_all)
              aa.append(x)
          }

          a = aa

        case _ =>
      }

    StateOfWorld.create(a.toArray)
  }

}