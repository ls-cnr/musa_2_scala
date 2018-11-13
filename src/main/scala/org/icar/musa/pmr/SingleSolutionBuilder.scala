package org.icar.musa.pmr

class SingleSolutionBuilder extends AbstractSolutionBuilder {

  private var partial_solution = new Solution()
  var solution = new Solution()

  override def evaluate_complete_sequence(sequence: StateSequence, interpr: SequenceInterpretation): Unit = {
    val s = partial_solution_from_sequence(sequence,interpr)

    if (s.isDefined) {
      partial_solution.blend(s.get)
      if (partial_solution.check_completeness) {
        solution = partial_solution.optimize
        solution.complete = true
      }

    }
  }

}
