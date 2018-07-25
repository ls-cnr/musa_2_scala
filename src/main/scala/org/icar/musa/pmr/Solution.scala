package org.icar.musa.pmr

import org.icar.fol.{AlwaysTrue, FOLCondition}
import org.icar.musa.spec.AbstractCapability

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
  def arcs_incoming_to(wfItem: WfItem) : Array[WfFlow] = {
    var in = List[WfFlow]()
    for (a <- arcs if a.to==wfItem)
      in = a :: in

    in.toArray
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




/* companion object */

object Solution {

  def build_from_simple_sequence(s : StateSequence, cap_map : Map[ (String,String),AbstractCapability]) : Option[Solution] = {
    val sol = new Solution()

    if (s.loop)
      None
    else {
      var last : WfItem = sol.start

      for (i <- s.seq.indices) {
        if (i>0) {
          val start = s.seq(i-1)
          val end = s.seq(i)
          val cap = cap_map(start,end)
          val task = WfTask(cap)
          sol.tasks += task

          val flow = WfFlow(last,task)
          sol.arcs += flow
          last = task
        }
      }

      val flow = WfFlow(last,sol.end)
      sol.arcs += flow

      sol.complete = true

      Some(sol)
    }
  }

  def build_from_xor_sequence(s: StateSequence, cap_map: Map[(String, String), AbstractCapability], scenario_map: Map[String, Array[String]], decision_map: Map[(String, String), DecisionPoint]): Option[Solution] = {

    if (!s.check_safeness(decision_map))
      None

    else {

      val sol = new Solution()
      var last: WfItem = sol.start
      var last_scenario = ""

      for (i <- s.seq.indices) {
        if (i > 0) {
          val start = s.seq(i - 1)
          val end = s.seq(i)
          val contain_xor = decision_map.contains((start,end))
          val cap = cap_map(start, end)

          if (!contain_xor) {
            val task = WfTask(cap)
            sol.tasks += task

            val flow = if (last.isInstanceOf[WfGateway]) WfFlow(last, task, last_scenario) else WfFlow(last, task)
            sol.arcs += flow
            last = task

          } else {

            val decision = decision_map((start,end))
            val xor_name = decision.map_ref

            val task = WfTask(cap)
            sol.tasks += task
            val gateway = WfGateway(xor_name, scenario_map(xor_name))
            sol.gateways += gateway

            val flow1 = if (last.isInstanceOf[WfGateway]) WfFlow(last, task, last_scenario) else WfFlow(last, task)
            val flow2 = WfFlow(task, gateway)
            sol.arcs += flow1
            sol.arcs += flow2

            last = gateway
            last_scenario = decision.scenario

          }


        }
      }

      val end_flow = if (last.isInstanceOf[WfGateway]) WfFlow(last, sol.end, last_scenario) else WfFlow(last, sol.end)
      sol.arcs += end_flow

      Some(sol)
    }
  }

  def merge_xor_sequences(s1 : Solution, s2 : Solution) : Option[Solution] = {
    //println("S1")
    //s1.print_for_graphviz()
    //println()
    //println("S2")
    //s2.print_for_graphviz()
    val (f1,f2) = compare_until_difference(s1,s2)
    if (f1.isInstanceOf[WfGateway]) {
      val scen = s2.arcs_out_from(f2).head.decision
      if (scenario_is_missing(s1,f1,scen)) {

        clone_and_merge(s1,f1,s2,f2)

      } else {
        None
      }

    } else {
      None
    }
  }

  private def scenario_is_missing(s: Solution, f1: WfItem, scen:String) : Boolean = {
    val arcs = s.arcs_out_from(f1)
    var cont = true
    for (a <- arcs if a.decision==scen) cont = false

    cont
  }

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

  def check_all_gateways(s: Solution): Boolean = {
    var test=true
    for (g<-s.gateways) {
      val out = s.arcs_out_from(g)
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
case class WfGateway(name : String, options: Array[String]) extends WfItem {
  override def hashCode(): Int = name.hashCode()
}
case class WfFlow(from: WfItem, to: WfItem, decision : String ="", condition: FOLCondition=FOLCondition(AlwaysTrue())) {
  override def hashCode(): Int = from.hashCode()+to.hashCode()+decision.hashCode
}

