package org.icar.musa.workflow

import org.icar.musa.pmr._

class WorkflowState(sol : Solution) {
  var current_tasks: Set[WfTask] = Set()
  var current_decisions: Set[WfGateway] = Set()

  init_from_start


  def init_from_start() : Unit = {
    val arcs = sol.arcs_out_from(sol.start)
    for (f <- arcs)
      add_following_items(f)
  }

  def add_following_items(f: WfFlow) : Unit = {
    f.to match {
      case x : WfTask => current_tasks += x
      case x : WfGateway => current_decisions += x
      case _ =>
    }
  }

  def completed(abs_name : String) : Unit = {
    for (t <- current_tasks if t.cap.name==abs_name)
      completed(t)
  }

  def completed(t : WfTask) : Unit = {
    current_tasks -= t
    val arcs = sol.arcs_out_from(t)
    for (f <- arcs)
      add_following_items(f)
  }

  def take_scenario(scn : String) : Unit = {
    for (g <- current_decisions)
      if (g.options.contains(scn)) {
        val arcs = sol.arcs_out_from(g)
        for (f <- arcs if f.decision==scn)
          add_following_items(f)
      }
  }

  def print_current_tasks() : Unit = {
    for (t <- current_tasks)
      println(t.cap.name)
  }

}
