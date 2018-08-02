package org.icar.musa.scenarios
import junit.framework.TestCase
import org.icar.fol._
import org.icar.ltl.{Finally, Globally, LogicAtom, LogicDisjunction}
import org.icar.musa.context.{AddEvoOperator, EvoOperator, StateOfWorld}
import org.icar.musa.pmr._
import org.icar.musa.spec.{AbstractCapability, EvolutionScenario, GroundedAbstractCapability, LTLGoal}

class OCCPTest extends OCCPScenario {

  def testDomain (): Unit = {
    val sol_builder = new MultiSolutionBuilder
    val wtsbuilder = new WTSLocalBuilder(problem,initial_state,capabilities,termination,sol_builder)
    wtsbuilder.build_wts()

    for (comp <- wtsbuilder.sol_builder.complete)
      println(comp)

    //wtsbuilder.sol_builder.log_mapping()

    wtsbuilder.wts.print_for_graphviz(wtsbuilder.sol_builder.pretty_print)
    //wtsbuilder.wts.print_for_graphviz(problem.asset.pretty_string)

    for (sol <- sol_builder.complete_solution)
      sol.print_for_graphviz()

      //sol.print_for_graphviz()
    /*for (seq <- wtsbuilder.sol_builder.partial) {
      val sol = Solution.build_from_xor_sequence(seq,wtsbuilder.sol_builder.cap_map, wtsbuilder.sol_builder.scenario_map)
      if (sol.isDefined)
        sol.get.print_for_graphviz()
    }*/
  }

}
