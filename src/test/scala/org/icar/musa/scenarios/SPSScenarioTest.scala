package org.icar.musa.scenarios

import junit.framework.TestCase
import org.icar.musa.pmr._

class SPSScenarioTest extends TestCase {

  val domain = new SPSScenario("./data/sps_data")
  lazy val assumptions = domain.assumption_set
  val sol_builder = new EarlyDecisionSolutionBuilder

  def testElements (): Unit = {

    println(domain.initial_state)
    println(domain.quality_asset.pretty_string(domain.initial_state))


    for (c <- domain.capabilities)
      println(c)
  }

  def testDomain (): Unit = {

    val wtsbuilder = new WTSLocalBuilder(domain.problem,domain.initial_state,domain.capabilities,IterationTermination(10),sol_builder) //termination)
    wtsbuilder.build_wts()

    for (comp <- wtsbuilder.sol_builder.complete)
      println(comp)

    //wtsbuilder.sol_builder.log_mapping()

    //wtsbuilder.wts.print_for_graphviz(domain.quality_asset.pretty_string)
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
