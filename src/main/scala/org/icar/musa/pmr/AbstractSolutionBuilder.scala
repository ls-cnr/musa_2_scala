package org.icar.musa.pmr

abstract class AbstractSolutionBuilder {

  def evaluate_complete_sequence(sequence: StateSequence, interpr : SequenceInterpretation): Unit

  def partial_solution_from_sequence(sequence: StateSequence, interpr : SequenceInterpretation) : Option[Solution] = {
    //require(interpr.decision_map.nonEmpty)

    if (!sequence.contain_xor(interpr.decision_map))
      solution_from_simple_sequence(sequence,interpr)
    else
      solution_from_xor_sequence(sequence,interpr)
  }

  def solution_from_simple_sequence(s : StateSequence, interpr : SequenceInterpretation) : Option[Solution] = {
    require(interpr.cap_map.nonEmpty)

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
    require(interpr.cap_map.nonEmpty && interpr.decision_map.nonEmpty && interpr.scenario_map.nonEmpty)

    if (!s.check_safeness(interpr.decision_map))
      None

    else {

      val sol = new Solution()
      var last: WfItem = sol.start
      var last_scenario = ""

      for (i <- s.seq.indices) {
        /* skip the first element: s0 */
        if (i > 0) {
          val start = s.seq(i - 1)
          val end = s.seq(i)
          val contain_xor = interpr.decision_map.contains((start,end))
          val cap = interpr.cap_map(start, end)

          if (!contain_xor) {
            val task = WfTask(cap)
            sol.tasks += task

            val flow =
              if (last.isInstanceOf[WfGateway]) WfFlow(last, task, last_scenario)
              else WfFlow(last, task)
            sol.arcs += flow

            last = task

          } else {

            val decision = interpr.decision_map((start,end))
            val xor_name = decision.map_ref

            val task = WfTask(cap)
            sol.tasks += task

            val gateway = WfGateway(xor_name, interpr.scenario_map(xor_name))
            sol.gateways += gateway

            val flow1 =
              if (last.isInstanceOf[WfGateway]) WfFlow(last, task, last_scenario)
              else WfFlow(last, task)
            val flow2 = WfFlow(task, gateway)
            sol.arcs += flow1
            sol.arcs += flow2

            last = gateway
            last_scenario = decision.scenario

          }


        }
      }

      val end_flow =
        if (last.isInstanceOf[WfGateway]) WfFlow(last, sol.end, last_scenario)
        else WfFlow(last, sol.end)
      sol.arcs += end_flow

      Some(sol)
    }
  }

}




