package org.icar.musa.scenarios

import junit.framework.TestCase
import org.icar.fol._
import org.icar.ltl._
import org.icar.musa.context.{AddEvoOperator, EvoOperator, RemoveEvoOperator, StateOfWorld}
import org.icar.musa.pmr._
import org.icar.musa.spec.TestLTLParser.{cap_specification, parseAll}
import org.icar.musa.spec.{AbstractCapability, EvolutionScenario, GroundedAbstractCapability, LTLGoal}

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

class PRINWakeUpTest extends TestCase {

  val domain = new PRINWakeUpScenario()
  val sol_builder = new SingleSolutionBuilder

  def testDomain (): Unit = {

    val wtsbuilder = new WTSLocalBuilder(domain.problem,domain.initial_state,domain.capabilities,domain.termination,sol_builder)
    wtsbuilder.build_wts()

    println("TERMINATED")

    for (comp <- wtsbuilder.sol_builder.complete)
      println(comp)

    //wtsbuilder.sol_builder.log_mapping()

    //wtsbuilder.wts.print_for_graphviz(wtsbuilder.sol_builder.pretty_print)
    wtsbuilder.wts.print_for_graphviz(domain.problem.asset.pretty_string)

    sol_builder.solution.print_for_graphviz()

    //sol.print_for_graphviz()
    /*for (seq <- wtsbuilder.sol_builder.partial) {
      val sol = Solution.build_from_xor_sequence(seq,wtsbuilder.sol_builder.cap_map, wtsbuilder.sol_builder.scenario_map)
      if (sol.isDefined)
        sol.get.print_for_graphviz()
    }*/
  }

}
