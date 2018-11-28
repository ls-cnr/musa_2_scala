package org.icar.musa.pmr

class MultiSolutionBuilder extends AbstractSolutionBuilder {

  var partial_solution_stack : Set[Solution] = Set()
  var complete_solution : Set[Solution] = Set()
  var new_solutions : List[Solution] = List()

  override def evaluate_complete_sequence(sequence: StateSequence, interpr : SequenceInterpretation): Unit = {
    if (interpr.cap_map.nonEmpty) {

      //println("SEQ: "+sequence)

      // search for new solutions
      val s = partial_solution_from_sequence(sequence,interpr)

      if (s.isDefined) {
        //println("partial sol!")
        //s.get.print_for_graphviz()
        add_solution_to_stack(s.get)
      }
    }

  }


  private def add_solution_to_stack(s : Solution) : Unit = {

    var to_add = List[Solution](s)

    for (sol <- partial_solution_stack) {
      val merge = Solution.merge_two_partial_solutions(sol,s)
      if (merge.isDefined) {
        //println("merged!")
        //println(merge.get.print_for_graphviz())
        to_add = merge.get :: to_add
      }
    }

    for (sol <- to_add) {
      partial_solution_stack += sol
      if (sol.check_completeness && sol.check_soundness) {
        complete_solution += sol
        new_solutions = sol :: new_solutions
      }
    }
    //println("TOTAL PARTIAL SOL= "+partial_solution_stack.size)
  }


}