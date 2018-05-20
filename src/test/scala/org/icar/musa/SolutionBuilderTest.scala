package org.icar.musa

import junit.framework.Assert.assertEquals
import junit.framework.TestCase
import org.icar.fol._
import org.icar.ltl.supervisor.NetSupervisor
import org.icar.ltl.{Finally, Globally, LogicAtom, LogicConjunction}
import org.icar.musa.context.{AddEvoOperator, EvoOperator, StateOfWorld}
import org.icar.musa.pmr._
import org.icar.musa.spec.{AbstractCapability, EvolutionScenario, GroundedAbstractCapability, LTLGoal}

import scala.collection.mutable.ArrayBuffer



class SolutionBuilderTest extends TestCase {

  def testUpdateSeq (): Unit = {
    val w = StateOfWorld.create(GroundPredicate("attach",AtomTerm("doc")))
    val f1 = Globally(LogicAtom("document",AtomTerm("doc")))
    val f2 = Finally(LogicAtom("ready",AtomTerm("doc")))
    val f3 = LogicConjunction(f1,f2)
    val su = NetSupervisor.initialize(f3,w,AssumptionSet())

    val sol_builder = new SolutionBuilder(WTSStateNode(w,su,0))

    //sol_builder.log_state

    //println("s0->A")
    sol_builder.update_seq_with("s0","A")
    //println("A->B")
    sol_builder.update_seq_with("A","B")

    //sol_builder.log_state

    assertEquals( true , sol_builder.partial.contains(Sequence(ArrayBuffer("s0","A","B"),false,false)) )

    //println("A->C")
    sol_builder.update_seq_with("A","C")

    //sol_builder.log_state

    assertEquals( true , sol_builder.partial.contains(Sequence(ArrayBuffer("s0","A","B"),false,false)) )
    assertEquals( true , sol_builder.partial.contains(Sequence(ArrayBuffer("s0","A","C"),false,false)) )

    //println("B->D")
    sol_builder.update_seq_with("B","D")
    //sol_builder.log_state


    //println("s0->B")
    sol_builder.update_seq_with("s0","B")
    //sol_builder.log_state
    assertEquals( true , sol_builder.partial.contains(Sequence(ArrayBuffer("s0","A","C"),false,false)) )
    assertEquals( true , sol_builder.partial.contains(Sequence(ArrayBuffer("s0","A","B","D"),false,false)) )
    assertEquals( false , sol_builder.partial.contains(Sequence(ArrayBuffer("s0","B"),false,false)) )
    assertEquals( true , sol_builder.partial.contains(Sequence(ArrayBuffer("s0","B","D"),false,false)) )

    //println("D->A")
    sol_builder.update_seq_with("D","A")
    //sol_builder.log_state

    assertEquals( true , sol_builder.partial.contains(Sequence(ArrayBuffer("s0","A","B","D","A"),true,false)) )
    assertEquals( true , sol_builder.partial.contains(Sequence(ArrayBuffer("s0","B","D","A","B"),true,false)) )
    assertEquals( true , sol_builder.partial.contains(Sequence(ArrayBuffer("s0","B","D","A","C"),false,false)) )
    assertEquals( true , sol_builder.partial.contains(Sequence(ArrayBuffer("s0","A","C"),false,false)) )

    //println("D->s0")
    sol_builder.update_seq_with("D","s0")
    //sol_builder.log_state

    //println("D->E (exit)")
    sol_builder.update_seq_with("D","E",true)
    //sol_builder.log_state

  }

  def testUpdateSeqWithForward (): Unit = {
    val w = StateOfWorld.create(GroundPredicate("attach", AtomTerm("doc")))
    val f1 = Globally(LogicAtom("document", AtomTerm("doc")))
    val f2 = Finally(LogicAtom("ready", AtomTerm("doc")))
    val f3 = LogicConjunction(f1, f2)
    val su = NetSupervisor.initialize(f3, w, AssumptionSet())

    val sol_builder = new SolutionBuilder(WTSStateNode(w, su, 0))


    sol_builder.update_seq_with("s0", "B")
    sol_builder.update_seq_with("B", "C")
    sol_builder.update_seq_with("C", "D")

    sol_builder.update_seq_with("s0", "A")

    sol_builder.update_seq_with("A", "B")

    assertEquals( true , sol_builder.partial.contains(Sequence(ArrayBuffer("s0","B","C","D"),false,false)) )
    assertEquals( true , sol_builder.partial.contains(Sequence(ArrayBuffer("s0","A","B","C","D"),false,false)) )
  }

  def testUpdateMultiExpansion (): Unit = {
    val w0 = StateOfWorld.create(GroundPredicate("a", AtomTerm("a")))

    val f1 = Globally(LogicAtom("document", AtomTerm("doc")))
    val f2 = Finally(LogicAtom("ready", AtomTerm("doc")))
    val f3 = LogicConjunction(f1, f2)
    val su = NetSupervisor.initialize(f3, w0, AssumptionSet())
    val sol_builder = new SolutionBuilder(WTSStateNode(w0, su, 0))

    sol_builder.update_seq_with("s0", "A")
    val x1 = sol_builder.add_xor(Array("scen1","scen2"))
    sol_builder.update_seq_with("A", x1)
    //sol_builder.update_seq_with(x1,"scen1")
    //sol_builder.update_seq_with(x1,"scen2")
    sol_builder.partial = sol_builder.partial + Sequence(ArrayBuffer[String]("x0.scen1"),false,false)
    sol_builder.partial = sol_builder.partial + Sequence(ArrayBuffer[String]("x0.scen2"),false,false)
    sol_builder.update_seq_with("x0.scen1", "B")
    sol_builder.update_seq_with("x0.scen2", "C")

    //sol_builder.log_state

  }

  def test_with_capabilities() : Unit = {
    val w0 = StateOfWorld.create(GroundPredicate("a", AtomTerm("doc")))
    val f = Globally(LogicAtom("a", AtomTerm("doc")))
    val su = NetSupervisor.initialize(f, w0, AssumptionSet())
    val quality = new EmptyQualityAsset(AssumptionSet())
    val ps = SingleGoalProblemSpecification(AssumptionSet(),LTLGoal(f),quality)

    val pre = FOLCondition(Literal(Predicate("a", AtomTerm("doc"))))
    val post = FOLCondition(Literal(Predicate("b", AtomTerm("doc"))))
    val evo = EvolutionScenario(Array[EvoOperator](AddEvoOperator(GroundPredicate("b", AtomTerm("doc")))))
    val c1 = GroundedAbstractCapability("capAB",pre,post,Map("1"-> evo))


    val pre2 = FOLCondition(Literal(Predicate("b", AtomTerm("doc"))))
    val post2 = FOLCondition(Disjunction(Literal(Predicate("c", AtomTerm("doc"))),Literal(Predicate("d", AtomTerm("doc")))))
    val evo2_1 = EvolutionScenario(Array[EvoOperator](AddEvoOperator(GroundPredicate("c", AtomTerm("doc")))))
    val evo2_2 = EvolutionScenario(Array[EvoOperator](AddEvoOperator(GroundPredicate("d", AtomTerm("doc")))))
    val c2 = GroundedAbstractCapability("capBCxD",pre2,post2,Map("scenc"-> evo2_1,"scend"-> evo2_2))

    val cap_set = Array[AbstractCapability](c1,c2)

    val expl = new SingleGoalProblemExploration(ps, w0, cap_set)


    val sol_builder = new SolutionBuilder(WTSStateNode(w0, su, 0))
    //sol_builder.log_state

    expl.execute_iteration()

    val exp : SimpleWTSExpansion = expl.highest_expansion.get.asInstanceOf[SimpleWTSExpansion]

    //println(exp)

    sol_builder.deal_with_expansion(exp)

    //sol_builder.log_state

    expl.pick_expansion(exp)
    expl.new_node(exp.end)
    expl.execute_iteration()

    val exp2 : MultiWTSExpansion = expl.highest_expansion.get.asInstanceOf[MultiWTSExpansion]
    //println(exp2)

    sol_builder.deal_with_multi_expansion(exp2)

    //sol_builder.log_state

  }

  def test_simple_solution_from_capabilities() : Unit = {
    val w0 = StateOfWorld.create(GroundPredicate("a", AtomTerm("doc")))
    val f = Finally(LogicAtom("d", AtomTerm("doc")))
    val su = NetSupervisor.initialize(f, w0, AssumptionSet())
    val quality = new EmptyQualityAsset(AssumptionSet())
    val ps = SingleGoalProblemSpecification(AssumptionSet(),LTLGoal(f),quality)

    val pre = FOLCondition(Literal(Predicate("a", AtomTerm("doc"))))
    val post = FOLCondition(Literal(Predicate("b", AtomTerm("doc"))))
    val evo = EvolutionScenario(Array[EvoOperator](AddEvoOperator(GroundPredicate("b", AtomTerm("doc")))))
    val c1 = GroundedAbstractCapability("capAB",pre,post,Map("1"-> evo))

    val pre2 = FOLCondition(Literal(Predicate("b", AtomTerm("doc"))))
    val post2 = FOLCondition(Literal(Predicate("c", AtomTerm("doc"))))
    val evo2 = EvolutionScenario(Array[EvoOperator](AddEvoOperator(GroundPredicate("c", AtomTerm("doc")))))
    val c2 = GroundedAbstractCapability("capBC",pre2,post2,Map("2"-> evo2))


    val pre3 = FOLCondition(Literal(Predicate("c", AtomTerm("doc"))))
    val post3 = FOLCondition(Literal(Predicate("d", AtomTerm("doc"))))
    val evo3 = EvolutionScenario(Array[EvoOperator](AddEvoOperator(GroundPredicate("d", AtomTerm("doc")))))
    val c3 = GroundedAbstractCapability("capCD",pre3,post3,Map("3"-> evo3))

    val cap_set = Array[AbstractCapability](c1,c2,c3)

    val expl = new SingleGoalProblemExploration(ps, w0, cap_set)
    val sol_builder = new SolutionBuilder(WTSStateNode(w0, su, 0))
    //sol_builder.log_state

    expl.execute_iteration()
    val exp : SimpleWTSExpansion = expl.highest_expansion.get.asInstanceOf[SimpleWTSExpansion]
    //println(exp)
    sol_builder.deal_with_expansion(exp)
    //sol_builder.log_state
    expl.pick_expansion(exp)
    expl.new_node(exp.end)

    expl.execute_iteration()
    val exp2 : SimpleWTSExpansion = expl.highest_expansion.get.asInstanceOf[SimpleWTSExpansion]
    //println(exp2)
    sol_builder.deal_with_expansion(exp2)
    //sol_builder.log_state
    expl.pick_expansion(exp2)
    expl.new_node(exp2.end)

    expl.execute_iteration()
    val exp3 : SimpleWTSExpansion = expl.highest_expansion.get.asInstanceOf[SimpleWTSExpansion]
    //println(exp2)
    sol_builder.deal_with_expansion(exp3)
    //sol_builder.log_state
    expl.pick_expansion(exp3)
    expl.new_node(exp3.end)


   for (s <- sol_builder.complete) {
     val sol = Solution.build_from_simple_sequence(s,sol_builder.cap_map)
     /*if (sol.isDefined)
        sol.get.print_for_graphviz*/

   }
  }


  def test_partial_solution_with_capabilities() : Unit = {
    val w0 = StateOfWorld.create(GroundPredicate("a", AtomTerm("doc")))
    val f = Globally(LogicAtom("a", AtomTerm("doc")))
    val su = NetSupervisor.initialize(f, w0, AssumptionSet())
    val quality = new EmptyQualityAsset(AssumptionSet())
    val ps = SingleGoalProblemSpecification(AssumptionSet(),LTLGoal(f),quality)

    val pre = FOLCondition(Literal(Predicate("a", AtomTerm("doc"))))
    val post = FOLCondition(Literal(Predicate("b", AtomTerm("doc"))))
    val evo = EvolutionScenario(Array[EvoOperator](AddEvoOperator(GroundPredicate("b", AtomTerm("doc")))))
    val c1 = GroundedAbstractCapability("capAB",pre,post,Map("1"-> evo))

    val pre2 = FOLCondition(Literal(Predicate("b", AtomTerm("doc"))))
    val post2 = FOLCondition(Disjunction(Literal(Predicate("c", AtomTerm("doc"))),Literal(Predicate("d", AtomTerm("doc")))))
    val evo2_1 = EvolutionScenario(Array[EvoOperator](AddEvoOperator(GroundPredicate("c", AtomTerm("doc")))))
    val evo2_2 = EvolutionScenario(Array[EvoOperator](AddEvoOperator(GroundPredicate("d", AtomTerm("doc")))))
    val c2 = GroundedAbstractCapability("capBCxD",pre2,post2,Map("scenc"-> evo2_1,"scend"-> evo2_2))

    val pre3 = FOLCondition(Literal(Predicate("c", AtomTerm("doc"))))
    val post3 = FOLCondition(Literal(Predicate("e", AtomTerm("doc"))))
    val evo3 = EvolutionScenario(Array[EvoOperator](AddEvoOperator(GroundPredicate("e", AtomTerm("doc")))))
    val c3 = GroundedAbstractCapability("capCE",pre3,post3,Map("1"-> evo3))

    val pre4 = FOLCondition(Literal(Predicate("d", AtomTerm("doc"))))
    val post4 = FOLCondition(Literal(Predicate("f", AtomTerm("doc"))))
    val evo4 = EvolutionScenario(Array[EvoOperator](AddEvoOperator(GroundPredicate("f", AtomTerm("doc")))))
    val c4 = GroundedAbstractCapability("capDF",pre4,post4,Map("1"-> evo4))

    val cap_set = Array[AbstractCapability](c1,c2,c3,c4)

    val expl = new SingleGoalProblemExploration(ps, w0, cap_set)

    val local_builder = new WTSLocalBuilder(ps,w0,cap_set,MaxEmptyIterationTermination(10))
    local_builder.build_wts()

    //local_builder.wts.print_for_graphviz(quality.pretty_string)

    for (p <- local_builder.sol_builder.partial) {
      val sol = Solution.build_from_xor_sequence(p,local_builder.sol_builder.cap_map,local_builder.sol_builder.scenario_map)
      /*if (sol.isDefined)
        sol.get.print_for_graphviz*/
    }

  }


}
