package org.icar.musa.pmr

import org.icar.ltl.supervisor.NetSupervisor
import org.icar.musa.context.StateOfWorld
import org.icar.musa.spec.GroundedAbstractCapability

class WTS (var root:WTSStateNode) {
  var nodes: Set[WTSStateNode] = Set[WTSStateNode](root)
  var arcs: Set[WTSCapabilityArc] = Set[WTSCapabilityArc]()
  var multiarcs: Set[WTSScenarioArc] = Set[WTSScenarioArc]()

  def addExpansion(exp : WTSExpansion) : Unit = {
    exp match {

      case simple : SimpleWTSExpansion =>
        nodes = nodes + simple.start
        nodes = nodes + simple.end
        arcs = arcs + WTSCapabilityArc(simple.start,simple.end,simple.cap,simple.prov)


      case multi : MultiWTSExpansion =>
        nodes = nodes + multi.start
        for (n <- multi.evo.values)
          nodes = nodes + n
        multiarcs = multiarcs + WTSScenarioArc(multi.start,multi.evo, multi.cap, multi.prov)
    }
  }

  def outgoing_capabilities(n : WTSStateNode) : List[WTSCapabilityArc] = {
    var l = List[WTSCapabilityArc]()

    for (a <- arcs if a.in == n)
      l = a :: l

    l
  }
  def incoming_capabilities(n : WTSStateNode) : List[WTSCapabilityArc] = {
    var l = List[WTSCapabilityArc]()

    for (a <- arcs if a.out == n)
      l = a :: l

    l
  }
  def outgoing_scenarios(n : WTSStateNode) : List[WTSScenarioArc] = {
    var l = List[WTSScenarioArc]()

    for (a <- multiarcs if a.in == n)
      l = a :: l

    l
  }

  def print_for_graphviz( pretty_string: StateOfWorld => String) : Unit = {
    println("digraph WTS {")
    for (n <- nodes.toList) {
      print("\""+pretty_string(n.w)+"\"")
      if (n==root)
        println("[style=bold,color=yellow];")
      else if (n.su.isAccepted)
        println("[style=bold,color=green];")
      else
        println("[color=black];")
    }

    for (a <- arcs) {
      print("\""+pretty_string(a.in.w)+"\"")
      print("->")
      print("\""+pretty_string(a.out.w)+"\"")
      println("[label=\""+a.cap.name+"\"]")
    }

    var counter = 1
    for (a<-multiarcs) {
      //println("X"+counter)
      print("\""+pretty_string(a.in.w)+"\"")
      print("->")
      print("X"+counter)
      println("[label=\""+a.cap.name+"\"]")

      for (scenario <- a.out.keys) {
        print("X"+counter)
        print("->")
        print("\""+pretty_string(a.out(scenario).w)+"\"")
        println("[label=\""+scenario+"\"]")
      }
      counter = counter +1
    }
    println("}")
  }

}


case class WTSStateNode(w:StateOfWorld, su : NetSupervisor, qos : Float)
case class WTSCapabilityArc(in : WTSStateNode, out : WTSStateNode, cap : GroundedAbstractCapability, prov : String="empty")
case class WTSScenarioArc(in : WTSStateNode, out : Map[String,WTSStateNode], cap : GroundedAbstractCapability, prov : String="empty")

abstract class WTSExpansion(val order : Float)
case class SimpleWTSExpansion(start:WTSStateNode,end:WTSStateNode, cap:GroundedAbstractCapability,prov:String="empty") extends WTSExpansion(end.qos) {
  override def toString: String = "{("+start.w+" => "+end.w+" with "+cap.name+" ["+end.su.current_state+" dist="+end.su.distance_to_satisfaction+" ("+end.su.petrinets+")}"
}
case class MultiWTSExpansion(start:WTSStateNode, evo: Map[String,WTSStateNode], average_distance_to_sat: Float, average_qos:Float, cap:GroundedAbstractCapability, prov:String="empty") extends WTSExpansion(average_qos) {
  override def toString: String = {
    var end =""
    for (e <- evo)
      end += e._2+"->"+e._2.w+"["+e._2.su.current_state+"("+e._2.su.distance_to_satisfaction+")],"

    "{from("+start.w+" to ("+end+") with "+cap.name+" (score="+average_qos+")}"
  }
}




