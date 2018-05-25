package org.icar.musa.scenarios

import junit.framework.TestCase
import org.icar.fol._
import org.icar.musa.context.{AddEvoOperator, EvoOperator, RemoveEvoOperator, StateOfWorld}
import org.icar.musa.pmr.WTSLocalBuilder
import org.icar.musa.spec.{AbstractCapability, EvolutionScenario, GroundedAbstractCapability}

import scala.collection.mutable.ArrayBuffer

class SPSScenarioTest extends TestCase with TestScenario {

  var circuit : Circuit = new Circuit()
  var scenario : ReconfigurationScenario = new ReconfigurationScenario()
  var mission : Mission = new Mission()

  def testElements (): Unit = {

    circuit = Circuit.circuit3
    mission = Mission.circuit3_mission_1
    scenario = ReconfigurationScenario.scenario1

    for (a <- assumption_set.rules)
      println(a)

    println()

    println(initial_state)

    println()

    for (c <- capabilities)
      println(c)
  }

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
      list += Assumption(s.source.up+":-"+s.dest.up+", "+s.closed+".")
      list += Assumption(s.dest.up+":-"+s.source.up+", "+s.closed+".")
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

    for (r <- circuit.switcher) {
      var state = if (scenario.open_switchers.contains(r.id)) "open" else "closed"
      list += GroundPredicate(state,AtomTerm(r.id))
    }

    for (r<- circuit.generators) {
      val state = if (scenario.up_generators.contains(r.id)) "up" else if (scenario.generator_malfunctioning.contains(r.id)) "error" else "down"
      list += GroundPredicate(state,AtomTerm(r.id))
    }

    for (r <- scenario.failures) {
      list += GroundPredicate("f",AtomTerm(r))
    }

    StateOfWorld.create(list.toArray)
  }

  override def capabilities : Array[AbstractCapability] = {
    var cap_list = ArrayBuffer[AbstractCapability]()

    for (g <- circuit.generators if !scenario.generator_malfunctioning.contains(g)) {
      cap_list += generate_switch_on_generator(g.id)
      cap_list += generate_switch_off_generator(g.id)
    }

    for (g <- circuit.switcher if !scenario.switcher_malfunctioning.contains(g)) {
      if (circuit.sw_map.contains(g.id)) {
        val g2_name = circuit.sw_map(g.id)
        cap_list += generate_combinated_on_off_switcher(g.id,g2_name)
      } else {
        cap_list += generate_close_switcher(g.id)
        cap_list += generate_open_switcher(g.id)
      }
    }


    cap_list.toArray
  }

  private def generate_switch_on_generator(name : String) : GroundedAbstractCapability = {
    val generator_name = "switch_ON_"+name
    val generator = AtomTerm(name)
    val pre = FOLCondition(Literal(Predicate("off", generator )))
    val post = FOLCondition(Literal(Predicate("on", generator )))
    val evo_1 = EvolutionScenario(Array[EvoOperator](RemoveEvoOperator(GroundPredicate("off", generator)),AddEvoOperator(GroundPredicate("on", generator))))
    GroundedAbstractCapability(generator_name,pre,post,Map("1"-> evo_1))
  }
  private def generate_switch_off_generator(name : String) : GroundedAbstractCapability = {
    val generator_name = "switch_OFF_"+name
    val generator = AtomTerm(name)
    val pre = FOLCondition(Literal(Predicate("on", generator )))
    val post = FOLCondition(Literal(Predicate("off", generator )))
    val evo_1 = EvolutionScenario(Array[EvoOperator](RemoveEvoOperator(GroundPredicate("on", generator)),AddEvoOperator(GroundPredicate("off", generator))))
    GroundedAbstractCapability(generator_name,pre,post,Map("1"-> evo_1))
  }
  private def generate_close_switcher(name : String) : GroundedAbstractCapability = {
    val capname = "CLOSE_"+name
    val switcher = AtomTerm(name)
    val pre = FOLCondition(Literal(Predicate("open", switcher )))
    val post = FOLCondition(Literal(Predicate("closed", switcher )))
    val evo_1 = EvolutionScenario(Array[EvoOperator](RemoveEvoOperator(GroundPredicate("open", switcher)),AddEvoOperator(GroundPredicate("closed", switcher))))
    GroundedAbstractCapability(capname,pre,post,Map("1"-> evo_1))
  }
  private def generate_open_switcher(name : String) : GroundedAbstractCapability = {
    val capname = "OPEN_"+name
    val switcher = AtomTerm(name)
    val pre = FOLCondition(Literal(Predicate("closed", switcher )))
    val post = FOLCondition(Literal(Predicate("open", switcher )))
    val evo_1 = EvolutionScenario(Array[EvoOperator](RemoveEvoOperator(GroundPredicate("closed", switcher)),AddEvoOperator(GroundPredicate("open", switcher))))
    GroundedAbstractCapability(capname,pre,post,Map("1"-> evo_1))
  }
  private def generate_combinated_on_off_switcher(name1 : String, name2 : String) : GroundedAbstractCapability = {
    val capname = "CLOSE_"+name1+"_&_OPEN_"+name2
    val switcher1 = AtomTerm(name1)
    val switcher2 = AtomTerm(name2)
    val pre = FOLCondition(Conjunction(Literal(Predicate("open", switcher1 )),Literal(Predicate("closed", switcher2 ))))
    val post = FOLCondition(Conjunction(Literal(Predicate("closed", switcher1 )),Literal(Predicate("open", switcher2 ))))
    val evo_1 = EvolutionScenario(Array[EvoOperator](
      RemoveEvoOperator(GroundPredicate("open", switcher1)),
      AddEvoOperator(GroundPredicate("closed", switcher1)),
      RemoveEvoOperator(GroundPredicate("closed", switcher2)),
      AddEvoOperator(GroundPredicate("open", switcher2))
    ))
    GroundedAbstractCapability(capname,pre,post,Map("1"-> evo_1))
  }


  override def termination = ???
}
