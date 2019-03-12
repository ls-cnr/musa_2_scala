package org.icar.musa.pmr

/*
 * EX: SingleSolution
 * the result of the Builder is to generate 0..n independent solutions
 *
 */

class EarlyDecisionSolutionBuilder extends AbstractSolutionBuilder {

  var partial_solution_stack : Set[Solution] = Set()
  var complete_solution : Set[Solution] = Set()
  var new_solutions : List[Solution] = List()

  override def evaluate_complete_sequence(sequence: StateSequence, interpr : SequenceInterpretation): Unit = {
    if (interpr.cap_map.nonEmpty) {
      val s = partial_solution_from_sequence(sequence,interpr)
      if (s.isDefined)
        add_solution_path_to_stack(s.get,sequence,interpr)
    }
  }

  private def add_solution_path_to_stack(solution_path : Solution, sequence: StateSequence, interpr : SequenceInterpretation) : Unit = {

    var   to_add = List[Solution](solution_path)

    for (sol <- partial_solution_stack) {
      val merge = Solution.merge_partial_solution_with_solution_path(sol,solution_path)
      if (merge.isDefined) {
        //println("merged!")
        //println(merge.get.print_for_graphviz())
        to_add = merge.get :: to_add
      }
    }

    for (sol <- to_add) {
      partial_solution_stack += sol
      if (sol.check_completeness && sol.check_soundness) {
        sol.complete = true
        val state: WTSStateNode = interpr.node_map(sequence.seq.last)
        sol.final_state_of_world = Some(state.w)

        complete_solution += sol
        new_solutions = sol :: new_solutions
      }
    }
    //println("TOTAL PARTIAL SOL= "+partial_solution_stack.size)
  }

}


