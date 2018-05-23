package org.icar.musa.scenarios

import junit.framework.TestCase
import org.icar.fol.{Assumption, AssumptionSet, AtomTerm, GroundPredicate}
import org.icar.musa.context.StateOfWorld
import org.icar.musa.pmr.WTSLocalBuilder

import scala.collection.mutable.ArrayBuffer

class SPSScenarioTest extends TestCase with TestScenario {

  var circuit : Circuit = new Circuit()
  var failure : ReconfigurationScenario = new ReconfigurationScenario()
  var mission : Mission = new Mission()

  def testDomain (): Unit = {
    val wtsbuilder = new WTSLocalBuilder(problem,initial_state,capabilities,termination)
    wtsbuilder.build_wts()

    for (comp <- wtsbuilder.sol_builder.complete)
      println(comp)

    //wtsbuilder.sol_builder.log_mapping()

    wtsbuilder.wts.print_for_graphviz(wtsbuilder.sol_builder.pretty_print)
    //wtsbuilder.wts.print_for_graphviz(problem.asset.pretty_string)

    for (sol <- wtsbuilder.sol_builder.complete_solution)
      sol.print_for_graphviz()

    //sol.print_for_graphviz()
    /*for (seq <- wtsbuilder.sol_builder.partial) {
      val sol = Solution.build_from_xor_sequence(seq,wtsbuilder.sol_builder.cap_map, wtsbuilder.sol_builder.scenario_map)
      if (sol.isDefined)
        sol.get.print_for_graphviz()
    }*/
  }



  override def assumption_set : AssumptionSet = {
    /* standard assumptions */
    val list = ArrayBuffer[Assumption]()
    list += Assumption("off(X) :- load(X), not on(X).")
    list += Assumption("off(X) :- generator(X), not on(X).")
    list += Assumption("down(X) :- node(X), not up(X).")
    list += Assumption("open(X) :- switch(X), not closed(X).")

    for (c<-circuit.connections) {
      list += Assumption(c.source.up+":-"+c.dest.up+","+"not "+c.failure+".")
      list += Assumption(c.dest.up+":-"+c.source.up+","+"not "+c.failure+".")
    }
    for (s<-circuit.switcher) {
      list += Assumption("sw("+s.id+").")
      list += Assumption(s.source.up+":-"+s.dest.up+","+"not "+s.closed+".")
      list += Assumption(s.dest.up+":-"+s.source.up+","+"not "+s.closed+".")
    }
    for (g<-circuit.generators) {
      list += Assumption("generator("+g.id+").")
      list += Assumption(g.node.up+":-"+g.up+".")
    }
    for (l<-circuit.loads) {
      list += Assumption("load("+l.id+").")
      list += Assumption(l.node.up+":-"+l.up+".")
    }

    AssumptionSet(list: _*)
  }

  override def goal_specification = ???

  override def quality_asset = ???

  override def initial_state : StateOfWorld = {
    var list = ArrayBuffer[GroundPredicate]()

    for (r <- failure.switcher_state.keys) {
      var state = if (failure.switcher_state(r)) "closed" else "open"
      list += GroundPredicate(state,AtomTerm(r.id))
    }


    StateOfWorld.create(list.toArray)
  }

  override def capabilities = ???

  override def termination = ???
}
