package org.icar.musa.pmr

import org.icar.musa.pmr.Solution.compare_until_difference

class SingleSolutionBuilder extends AbstractSolutionBuilder {

  var solution = new Solution()

  var gw_it = 0;

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

  def blend_by_adding_a_gateway(sol: Solution, f: WfItem): Unit = {
    /* verify there is someting to blend */
    val main_out_from_f = solution.arcs_out_from(f)
    val seco_out_from_f = sol.arcs_out_from(f)
    if (main_out_from_f.length>0 && seco_out_from_f.length > 0) {
      val main_next = main_out_from_f.head.to
      val seco_next = seco_out_from_f.head.to

      /* elimino connessione con il resto del wf */
      solution.arcs -= main_out_from_f.head

      /* aggancio un gateway con scenario unico (non e' una scelta) */
      val gateway = WfGateway("G_"+gw_it,Array("unique_"+gw_it))
      solution.gateways += gateway
      gw_it += 1

      solution.arcs += WfFlow(f,gateway)
      solution.arcs += WfFlow(gateway,main_next)
      solution.arcs += WfFlow(gateway,seco_next)

      solution.blend(sol,seco_next)
    }

  }

  def blend_by_gateway_fusion(sol: Solution, f: WfItem): Unit = {
    var out_second_gateway = sol.arcs_out_from(f)
    for (flow <- out_second_gateway) {
      solution.arcs += flow
      solution.blend(sol,flow.to)
    }
/*
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
*/

  }



}
