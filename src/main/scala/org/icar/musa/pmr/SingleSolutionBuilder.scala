package org.icar.musa.pmr

import org.icar.musa.pmr.Solution.compare_until_difference

class SingleSolutionBuilder extends AbstractSolutionBuilder {

  var solution = new Solution()

  override def evaluate_complete_sequence(sequence: StateSequence, interpr: SequenceInterpretation): Unit = {


    val s = partial_solution_from_sequence(sequence,interpr)

    if (s.isDefined)
      blend_with_solution(s.get)
  }

  def blend_with_solution(sol: Solution) : Unit = {

    val f = Solution.last_equal_element(solution,sol)

    f match {
      case WfStartEvent() => blend_by_adding_a_gateway(sol,f)
      case WfTask(cap) => blend_by_adding_a_gateway(sol,f)
      case WfGateway(name,options) => blend_by_gateway_fusion(sol,f)
      case WfEndEvent() => /* do nothing */
    }

  }

  def blend_by_adding_a_gateway(sol: Solution, f: WfItem): Unit = ???

  def blend_by_gateway_fusion(sol: Solution, f: WfItem): Unit = {
    var out_second_gateway = sol.arcs_out_from(f)
    for (flow <- out_second_gateway)
      solution.arcs += flow

    var next = f
    var terminate = false
    while (terminate==false) {
      val opt_next = Solution.next_element(sol,next)
      if (opt_next.isDefined) {
        next = opt_next.get
        if (!next.isInstanceOf[WfEndEvent]) {

          solution.add(next)
          val outflows = sol.arcs_out_from(next)
          solution.add(outflows)

        } else {
          terminate = true
        }
      } else {
        terminate= true
      }

    }

  }



}
