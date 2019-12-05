package org.icar.musa.pmr

import org.icar.musa.context.StateOfWorld
import scala.collection.mutable.ArrayBuffer

class Solution() {

  val start = WfStartEvent()
  val end = WfEndEvent()

  var tasks: Set[WfTask] = Set()
  var gateways: Set[WfGateway] = Set()
  var arcs: Set[WfFlow] = Set()

  var complete : Boolean = false
  var final_state_of_world : Option[StateOfWorld] = None
  var final_goal_sat : Float = 0


  def inlineString : String = {
    if (!contains_gateways)
      to_inline(start,List())
    else
      "<complex gateway>"
  }

  def to_inline(e: WfItem, visited : List[WfTask]): String = {
    if (!visited.contains(e)) {
      val out = arcs_out_from(e)
      e match {
        case x:WfStartEvent => "START ->" + to_inline(out(0).to, visited )
        case x:WfEndEvent => "END"
        case x:WfTask => x.cap.name + " -> " + to_inline(out(0).to, x :: visited)
      }
    } else
      "<recursion error>"
  }


  /*def to_inline(e: WfItem): String = {
    var string = ""
    val outs : Array[WfFlow] = arcs_out_from(e)

    if (outs.length>0) {

    //if (outs.length == 1) {
      val next = outs.head.to
      next match {
        case x:WfTask => string += x.cap.name + " -> "
        case _ =>
      }
      string += to_inline(next)
    //}

/*
    if (outs.length > 1) {
      for (o <- outs) {
        string += "("
        val next = o.to
        next match { case x:WfTask => string += x.cap.name + " -> "}
        string += to_inline(next) + " | "
        string += ")"
      }
    }
*/
    }
    string
  }*/


  def contains_gateways : Boolean = gateways.nonEmpty

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

  def directly_connected(first: WfItem, second: WfItem): Boolean = {
    var flag = false
    val outs = arcs_out_from(first)
    for (o <- outs)
      if (o.to == second)
        flag = true

    flag
  }



  def blend(sol: Solution) : Unit = {
    arcs ++= sol.arcs
    tasks ++= sol.tasks
    gateways ++= sol.gateways
  }


  def optimize: Solution = {
    val sol = new Solution()
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

  def to_graphviz_string( ) : String = {
    var string = "digraph Solution {\n"

    for (x <- gateways)
      string += x.name+"[shape=diamond]\n"

    for (a <- arcs) {
      a.from match {
        case _: WfStartEvent => string += "start"
        case x : WfTask => string += "\""+x.cap.name+"\""
        case x : WfGateway => string += "\""+x.name+"\""
        case _: WfEndEvent => string += "end"
      }
      string += " -> "
      a.to match {
        case _: WfStartEvent => string += "start"
        case x : WfTask => string += "\""+x.cap.name+"\""
        case x : WfGateway => string += "\""+x.name+"\""
        case _: WfEndEvent => string += "end"
      }
      if (a.decision != "")
        string += "[label="+a.decision+"]\n"
      else
        string += "\n"
    }

    string += "}\n"

    string
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

  // preconditions:
  // sol is a simple sequence (always outs.length == 1)
  // curr and g are always contained in sol
  // the path from start to g exists
  def reverse_path_in_a_sequence(sol:Solution, curr: WfItem, g:WfItem, visited:List[WfItem]) : List[WfItem] = {
    if (curr==g)
      curr :: visited
    else {
      val next = sol.arcs_out_from(curr).head.to
      Solution.reverse_path_in_a_sequence(sol,next,g,curr :: visited)
    }

  }

  def check_if_reverse_path_is_contained(sol:Solution, revpath:List[WfItem]) : Boolean = {
    var flag = true
    if (revpath.length>1) {
      val first = revpath.head
      val second = revpath.tail.head

      //val outs = sol.arcs_out_from(second)
      if (sol.directly_connected(second,first))
        flag = check_if_reverse_path_is_contained(sol,revpath.tail.tail)

      else
        flag = false
    }

    flag
  }

  def merge_partial_solution_with_solution_path(partial_sol : Solution, solution_path : Solution) : Option[Solution] = {
    var compatible = true

    if (partial_sol.contains_gateways && solution_path.contains_gateways) {

      for (t <- solution_path.tasks if compatible) {
        if (partial_sol.tasks.contains(t))
          compatible = check_task_compatibility(partial_sol.arcs_out_from(t),solution_path.arcs_out_from(t))
      }

      if (compatible)
        for (g <- solution_path.gateways if compatible)
          if (partial_sol.gateways.contains(g))
            if (check_gateway_paths(solution_path,partial_sol,g))
              compatible = check_gateway_compatibility(g.options,partial_sol.arcs_out_from(g),solution_path.arcs_out_from(g))

      if (compatible) {
        val sol = new Solution()
        sol.tasks ++= partial_sol.tasks
        sol.tasks ++= solution_path.tasks
        sol.gateways ++= partial_sol.gateways
        sol.gateways ++= solution_path.gateways
        sol.arcs ++= partial_sol.arcs
        sol.arcs ++= solution_path.arcs

        Some( sol )
      } else {
        None
      }

    } else {
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


  /* check_gateway paths */
  def check_gateway_paths(path_sol1:Solution,sol2:Solution,g:WfGateway) : Boolean = {
    val revpath = Solution.reverse_path_in_a_sequence(path_sol1,path_sol1.start,g,List())
    Solution.check_if_reverse_path_is_contained(sol2,revpath)
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

}


