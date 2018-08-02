package org.icar.musa.pmr

import org.icar.fol.{AlwaysTrue, FOLCondition}
import org.icar.musa.spec.{AbstractCapability, GroundedAbstractCapability}

import scala.collection.mutable.ArrayBuffer

class Solution() {
  val start = WfStartEvent()
  val end = WfEndEvent()

  var tasks: Set[WfTask] = Set()
  var gateways: Set[WfGateway] = Set()
  var arcs: Set[WfFlow] = Set()

  var complete : Boolean = false

  def arcs_out_from(wfItem: WfItem) : Array[WfFlow] = {
    var out = List[WfFlow]()
    for (a <- arcs if a.from==wfItem)
      out = a :: out

    out.toArray
  }
  def arcs_out_from(wfItem: WfItem,scen : String) : Array[WfFlow] = {
    var out = List[WfFlow]()
    for (a <- arcs if a.from==wfItem && a.decision==scen)
      out = a :: out

    out.toArray
  }
  def arcs_incoming_to(wfItem: WfItem) : Array[WfFlow] = {
    var in = List[WfFlow]()
    for (a <- arcs if a.to==wfItem)
      in = a :: in

    in.toArray
  }

  def isEmpty: Boolean = arcs_out_from(start).length == 0

  def add(elem: WfItem) : Unit = {
    elem match {
      case e : WfStartEvent =>
      case e : WfEndEvent =>
      case g : WfGateway => gateways += g
      case t : WfTask => tasks += t
    }
  }
  def add(flows: Array[WfFlow]) : Unit = {
    for (f <- flows)
      arcs += f
  }
  def contained(elem: WfItem) : Boolean = {
    elem match {
      case e : WfStartEvent => true
      case e : WfEndEvent => true
      case g : WfGateway => gateways.contains(g)
      case t : WfTask => tasks.contains(t)
    }
  }

  def blend(sol: Solution) : Unit = {
    arcs ++= sol.arcs
    tasks ++= sol.tasks
    gateways ++= sol.gateways
  }


  def optimize: Solution = {
    var sol = new Solution()
    sol.tasks = Set[WfTask](tasks.toSeq: _*)
    sol.gateways = Set[WfGateway](gateways.toSeq: _*)
    sol.arcs = Set[WfFlow](arcs.toSeq: _*)

    var gw_it = 0

    /* optimize tasks with multiple outs */
    for (t <- sol.tasks) {
      val out = sol.arcs_out_from(t)
      if (out.length>1) {
        val gateway = WfGateway("G_"+gw_it,Array("unique_"+gw_it))
        gw_it +=1
        sol.gateways += gateway
        sol.arcs += WfFlow(t,gateway)

        for (o <- out) {
          sol.arcs -= o
          val next = o.to
          sol.arcs += WfFlow(gateway,next)
        }
      }
    }

    /* optimize gateways with repeated scenarios */
    for (g <- sol.gateways) {
      val scenarios = g.options
      for (s <- scenarios) {
        val out_scen = sol.arcs_out_from(g,s)
        if (out_scen.length>1) {
          val gateway = WfGateway("G_"+gw_it,Array("unique_"+gw_it))
          gw_it += 1
          sol.gateways += gateway
          sol.arcs += WfFlow(g,gateway,s)

          for (o <- out_scen) {
            sol.arcs -= o
            val next = o.to
            sol.arcs += WfFlow(gateway,next)
          }
        }
      }
    }

    sol
  }




  def blend(sol: Solution, focus: WfItem) : Unit = {
    if (!focus.isInstanceOf[WfEndEvent] && !contained(focus)) {
      add(focus)
      val out = sol.arcs_out_from(focus)
      for (f <- out) {
        arcs += f
        blend(sol,f.to)
      }
    }
  }

  /* it is complete if all gateways are complete and only the end node has not outgoing */
  def check_completeness: Boolean = {
    if (all_gateways_are_complete)
      if (only_end_has_not_outs)
        true
      else
      false
    else
      false
  }

  private def all_gateways_are_complete : Boolean = {
    var t = true
    for (g <- gateways) {
      if (arcs_out_from(g).length < g.options.length)
       t=false
    }

    t
  }

  private def only_end_has_not_outs : Boolean = {
    var result = true

    if (arcs_out_from(start).isEmpty)
      result=false

    for (t<-tasks if result==true)
      if (arcs_out_from(t).isEmpty)
        result=false

    for (g<-gateways if result==true)
      if (arcs_out_from(g).isEmpty)
        result=false

    result
  }

  /* it is sound if end node is reachable from any node */
  def check_soundness: Boolean = {
    val set : Set[WfItem] = reachable_from(end,Set())
    var result = true

    for (t<-tasks if result==true)
      if (!set.contains(t))
        result = false

    for (g<-gateways if result==true)
      if (!set.contains(g))
        result = false

    result
  }

  private def reachable_from(focus: WfItem, visited : Set[WfItem]): Set[WfItem] = {
    if (visited.contains(focus))
      visited
    else if (focus==start)
      visited
    else {
      var new_visited = visited
      new_visited += focus

      val arcs = arcs_incoming_to(focus)
      for (a<-arcs)
        new_visited ++= reachable_from(a.from,new_visited)

      new_visited
    }
  }


  private def to_end(node: WfItem, visited : ArrayBuffer[WfItem]) : Boolean = {
    if (visited.contains(node))
      false

    else {
      var new_visited = visited
      new_visited += node

      node match {
        case _: WfEndEvent => true
        case x : WfTask => to_end (arcs_out_from(x).head.to, new_visited)
        case x : WfGateway =>
          var result = false
          val outs = arcs_out_from(x)
          for (o <- outs if result == true)
            if (to_end(o.to, new_visited))
              result = true

          result
      }
    }
  }

  def check_all_gateways: Boolean = {
    var test=true
    for (g<-gateways) {
      val out = arcs_out_from(g)
      for (s<-g.options) {
        if (presences_of_options(out,s)>1)
          test=false
      }

    }
    test
  }


  def presences_of_options(out: Array[WfFlow],option : String) : Int = {
    var count = 0
    for (o <- out)
      if (o.decision==option)
        count += 1

    count
  }


  def print_for_graphviz( ) : Unit = {
    println("digraph Solution {")

    for (x <- gateways)
      println(x.name+"[shape=diamond]")

    for (a <- arcs) {
      a.from match {
        case _: WfStartEvent => print("start")
        case x : WfTask => print("\""+x.cap.name+"\"")
        case x : WfGateway => print("\""+x.name+"\"")
        case _: WfEndEvent => print("end")
      }
      print(" -> ")
      a.to match {
        case _: WfStartEvent => print("start")
        case x : WfTask => print("\""+x.cap.name+"\"")
        case x : WfGateway => print("\""+x.name+"\"")
        case _: WfEndEvent => print("end")
      }
      if (a.decision != "")
        println("[label=\""+a.decision+"\"]")
      else
        println()
    }

    println("}")
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[Solution]

  override def equals(other: Any): Boolean = other match {
    case that: Solution =>
      (that canEqual this) &&
        arcs == that.arcs &&
        complete == that.complete
    case _ => false
  }

  override def hashCode(): Int = {
    var sum = 0
    for (a <- arcs)
      sum += a.hashCode()

    sum
  }
}










object Solution {

  //still useful?
  def compare_until_difference(s1: Solution, s2: Solution): (WfItem, WfItem) = {
    var compatible = true
    var terminate = false

    var focus1: WfItem = s1.start
    var focus2: WfItem = s2.start

    while (compatible && !terminate) {
      val out_flows_from_s1 = s1.arcs_out_from(focus1)
      val out_flow_from_s2 = s2.arcs_out_from(focus2).head

      if (out_flows_from_s1.contains(out_flow_from_s2)) {
        /* update focus1 and focus 2 */
        for (f <- out_flows_from_s1 if f.decision == out_flow_from_s2.decision)
          focus1 = f.to
        focus2 = out_flow_from_s2.to

        /* check termination */
        terminate = (focus1.isInstanceOf[WfEndEvent] || focus2.isInstanceOf[WfEndEvent])
      } else {

        /* focus 1 and focus 2 have different outgoing flows */
        compatible = false
      }
    }

    (focus1, focus2)
  }


  //still useful?
  def last_equal_element(s1: Solution, s2: Solution): WfItem = {
    var compatible = true
    var terminate = false

    var focus1: WfItem = s1.start
    var focus2: WfItem = s2.start

    while (compatible && !terminate) {
      val opt_next2 = next_element(s2,focus2)
      if (!opt_next2.isDefined) {
        terminate=true
      } else {
        val next2 = opt_next2.get
        val opt_next1 = find_opt_next_element(s1,focus1,next2)
        if (!opt_next1.isDefined) {
          compatible = false
        } else {
          val next1 = opt_next1.get
          if (next1.isInstanceOf[WfEndEvent]) {
            terminate = true
          } else {
            focus1 = next1
            focus2 = next2
          }
        }
      }
    }

    focus1
  }

  //still useful?
  def next_element(s: Solution, focus: WfItem) : Option[WfItem] = {
    val out = s.arcs_out_from(focus)
    if (out.length>0)
      Some(out.head.to)
    else
      None
  }

  //still useful?
  def find_opt_next_element(s: Solution, focus: WfItem, next: WfItem) : Option[WfItem] = {
    var ret : Option[WfItem] = None

    for (succ_arc <- s.arcs_out_from(focus) if ret==None) {
      if ( succ_arc.to == next )
        ret = Some( succ_arc.to )
    }

    ret
  }


  def merge_two_partial_solutions(s1 : Solution, s2 : Solution) : Option[Solution] = {
    var compatible = true

    for (t <- s2.tasks if compatible) {
      if (s1.tasks.contains(t))
        compatible = check_task_compatibility(s1.arcs_out_from(t),s2.arcs_out_from(t))
    }

    if (compatible)
      for (g <- s2.gateways if compatible)
        if (s1.gateways.contains(g))
          compatible = check_gateway_compatibility(g.options,s1.arcs_out_from(g),s2.arcs_out_from(g))

    if (compatible) {
      var sol = new Solution()
      sol.tasks ++= s1.tasks
      sol.tasks ++= s2.tasks
      sol.gateways ++= s1.gateways
      sol.gateways ++= s2.gateways
      sol.arcs ++= s1.arcs
      sol.arcs ++= s2.arcs

      Some( sol )
    } else {
      /*println("//////////////")
      s1.print_for_graphviz()
      println("not compatible with")
      s2.print_for_graphviz()
      println("//////////////")*/
      None
    }
  }

  /* two tasks are compatible if when merged the result has only 1 outgoing arc */
  def check_task_compatibility(flows: Array[WfFlow], flows1: Array[WfFlow]): Boolean = {
    if (flows1.length == 1 && flows1.length == 1 && flows.head.to == flows1.head.to)
      true
    else
      false
  }

  /* two gateways are compatible if when merged the result has not duplicate scenario arcs */
  def check_gateway_compatibility(options: Array[String], flows: Array[WfFlow], flows1: Array[WfFlow]): Boolean = {
    var compatibility = true

    for (s <- options if compatibility==true) {
      val out_s1 = occurrence_scenario(flows,s)
      val out_s2 = occurrence_scenario(flows1,s)
      if (out_s1.isDefined && out_s2.isDefined) {
        val next1 = out_s1.get.to
        val next2 = out_s2.get.to
        if (next1 != next2 )
          compatibility=false
      }

    }

    compatibility
  }

  def occurrence_scenario(flows: Array[WfFlow], scen : String) : Option[WfFlow] = {
    var ret : Option[WfFlow] = None
    for (f <- flows if f.decision==scen)
      ret = Some(f)
    ret
  }

  //still useful?
  private def scenario_is_missing(s: Solution, f1: WfItem, scen:String) : Boolean = {
    val arcs = s.arcs_out_from(f1)
    var cont = true
    for (a <- arcs if a.decision==scen) cont = false

    cont
  }


  //still useful?
  private def clone_and_merge(s1: Solution, f1: WfItem, s2: Solution, f2: WfItem): Option[Solution] = {
    val s = new Solution()
    for (t <- s1.tasks)
      s.tasks += t
    for (g <- s1.gateways)
      s.gateways += g
    for (f <- s1.arcs)
      s.arcs += f

    for (t <- s2.tasks)
      s.tasks += t
    for (g <- s2.gateways)
      s.gateways += g
    for (f <- s2.arcs)
      s.arcs += f

    if (s.check_all_gateways)
      Some(s)
    else
      None
  }

  /*
  def compare_until_difference(s1 : Solution, s2 : Solution) : (WfItem,WfItem) = {
    var compatible = true
    var terminate = false

    var focus1 : WfItem = s1.start
    var focus2 : WfItem = s2.start

    while (compatible && !terminate) {
      val out_flows_from_s1 = s1.arcs_out_from(focus1)
      val out_flow_from_s2 = s2.arcs_out_from(focus2).head

      if (out_flows_from_s1.contains(out_flow_from_s2)) {
        /* update focus1 and focus 2 */
        for (f <- out_flows_from_s1 if f.decision==out_flow_from_s2.decision)
          focus1 = f.to
        focus2 = out_flow_from_s2.to

        /* check termination */
        terminate = (focus1.isInstanceOf[WfEndEvent] || focus2.isInstanceOf[WfEndEvent])
      } else {

        /* focus 1 and focus 2 have different outgoing flows */
        compatible = false
      }
    }

    (focus1,focus2)
  }

  private def clone_and_merge(s1 : Solution, f1:WfItem, s2 : Solution, f2:WfItem) : Option[Solution] = {
    val s = new Solution()
    for (t <- s1.tasks)
      s.tasks += t
    for (g <- s1.gateways)
      s.gateways += g
    for (f <- s1.arcs)
      s.arcs += f

    for (t <- s2.tasks)
      s.tasks += t
    for (g <- s2.gateways)
      s.gateways += g
    for (f <- s2.arcs)
      s.arcs += f

    if (check_all_gateways(s))
      Some(s)
    else
      None
  }
*/
}



abstract class WfItem
abstract class WfEvent extends WfItem
case class WfStartEvent() extends WfEvent {
  override def hashCode(): Int = "start".hashCode()
}
case class WfEndEvent() extends WfEvent {
  override def hashCode(): Int = "end".hashCode()
}
case class WfTask(cap : AbstractCapability) extends WfItem {
  override def hashCode(): Int = cap.name.hashCode()
}
object WfTask {
  def dummy(cap : String) = WfTask(GroundedAbstractCapability(cap,null,null,null,null))
}
case class WfGateway(name : String, options: Array[String]) extends WfItem {
  override def hashCode(): Int = name.hashCode()
}
case class WfFlow(from: WfItem, to: WfItem, decision : String ="", condition: FOLCondition=FOLCondition(AlwaysTrue())) {
  override def hashCode(): Int = from.hashCode()+to.hashCode()+decision.hashCode
}

