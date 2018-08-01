package org.icar.musa.pmr

abstract class SolutionBuilder {

  def evaluate_complete_sequence(sequence: StateSequence, interpr : SequenceInterpretation): Unit

}


class MultiSolutionBuilder extends SolutionBuilder {

  var incomplete_solution_stack : Set[Solution] = Set()
  var complete_solution : Set[Solution] = Set()
  var new_solutions : List[Solution] = List()

  override def evaluate_complete_sequence(sequence: StateSequence, interpr : SequenceInterpretation): Unit = {
    if (interpr.cap_map.nonEmpty) {

      // search for new solutions
      val s =
        if (!sequence.contain_xor(interpr.decision_map))
          solution_from_simple_sequence(sequence,interpr)
        else
          solution_from_xor_sequence(sequence,interpr)

      if (s.isDefined)
        add_solution_to_stack(s.get)
    }

  }


  private def add_solution_to_stack(s : Solution) : Unit = {
    var to_add = List[Solution](s)

    for (sol <- incomplete_solution_stack) {
      val merge = merge_xor_sequences(sol,s)
      if (merge.isDefined)
        to_add = merge.get :: to_add
    }

    for (sol <- to_add) {
      incomplete_solution_stack += sol
      if (sol.check_completeness && s.check_soundness) {
        complete_solution += sol
        new_solutions = sol :: new_solutions
      }
    }
  }


  def solution_from_simple_sequence(s : StateSequence, interpr : SequenceInterpretation) : Option[Solution] = {
    val sol = new Solution()

    if (s.loop)
      None
    else {
      var last : WfItem = sol.start

      for (i <- s.seq.indices) {
        if (i>0) {
          val start = s.seq(i-1)
          val end = s.seq(i)
          val cap = interpr.cap_map(start,end)
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

  def solution_from_xor_sequence(s: StateSequence, interpr : SequenceInterpretation): Option[Solution] = {

    if (!s.check_safeness(interpr.decision_map))
      None

    else {

      val sol = new Solution()
      var last: WfItem = sol.start
      var last_scenario = ""

      for (i <- s.seq.indices) {
        if (i > 0) {
          val start = s.seq(i - 1)
          val end = s.seq(i)
          val contain_xor = interpr.decision_map.contains((start,end))
          val cap = interpr.cap_map(start, end)

          if (!contain_xor) {
            val task = WfTask(cap)
            sol.tasks += task

            val flow = if (last.isInstanceOf[WfGateway]) WfFlow(last, task, last_scenario) else WfFlow(last, task)
            sol.arcs += flow
            last = task

          } else {

            val decision = interpr.decision_map((start,end))
            val xor_name = decision.map_ref

            val task = WfTask(cap)
            sol.tasks += task
            val gateway = WfGateway(xor_name, interpr.scenario_map(xor_name))
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