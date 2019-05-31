package org.icar.musa.scenarios

import junit.framework.TestCase
import org.icar.fol._
import org.icar.ltl.{Finally, LogicConjunction, ltlFormula}
import org.icar.musa.context.{AddEvoOperator, EvoOperator, RemoveEvoOperator, StateOfWorld}
import org.icar.musa.pmr._
import org.icar.musa.scenarios.sps.{Circuit, Mission, ReconfigurationScenario}
import org.icar.musa.main_entity.{AbstractCapability, EvolutionScenario, GroundedAbstractCapability, LTLGoal}

import scala.collection.mutable.ArrayBuffer


class SPS_Reconfiguration_test extends TestCase {

  val domain = new SPSScenario("")
  lazy val assumptions = domain.assumption_set
  val circuit = Circuit.load_from_file("./data/sps_data/circuit3.txt")
  val mission = Mission.circuit3_file_mission_1
  val scenario = ReconfigurationScenario.scenario_circuit3_parsed_1

  val sol_builder = new EarlyDecisionSolutionBuilder

  def testElements (): Unit = {


    /*
        for (a <- assumptions.rules)
          println(a)

        println()


        for (c <- capabilities)
          println(c)

        println(goal_specification)
    */

    //println(circuit.print_for_graphviz)

    println(domain.initial_state)
    //    println(quality_asset.evaluate_state(initial_state))
    //    println(quality_asset.max_score)

    println(domain.quality_asset.pretty_string(domain.initial_state))

  }

  def testDomain (): Unit = {
    //circuit = Circuit.circuit3
    //mission = Mission.circuit3_mission_1
    //scenario = ReconfigurationScenario.scenario1

    //circuit.print_for_graphviz

    val wtsbuilder = new WTSLocalBuilder(domain.problem,domain.initial_state,domain.capabilities,IterationTermination(10),sol_builder) //termination)
    wtsbuilder.build_wts()

    for (comp <- wtsbuilder.sol_builder.complete)
      println(comp)

    //wtsbuilder.sol_builder.log_mapping()

    wtsbuilder.wts.print_for_graphviz(domain.quality_asset.pretty_string)
    wtsbuilder.wts.print_for_graphviz( x => x.toString )

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
