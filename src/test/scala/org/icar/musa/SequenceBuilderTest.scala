package org.icar.musa

import junit.framework.Assert.assertEquals
import junit.framework.TestCase
import org.icar.fol._
import org.icar.ltl.supervisor.{NetSupervisor, SupervisorBuilder}
import org.icar.ltl.{Finally, Globally, LogicAtom, LogicConjunction}
import org.icar.musa.context.{AddEvoOperator, EvoOperator, StateOfWorld}
import org.icar.musa.pmr._
import org.icar.musa.main_entity.{AbstractCapability, EvolutionScenario, GroundedAbstractCapability, LTLGoal}

import scala.collection.mutable.ArrayBuffer



class SequenceBuilderTest extends TestCase {
  val sol_builder = new EarlyDecisionSolutionBuilder

  def testUpdateSeq (): Unit = {
    val w = StateOfWorld.create(GroundPredicate("attach",AtomTerm("doc")))
    val f1 = Globally(LogicAtom("document",AtomTerm("doc")))
    val f2 = Finally(LogicAtom("ready",AtomTerm("doc")))
    val f3 = LogicConjunction(f1,f2)
    val builder = new SupervisorBuilder()
    val su = builder.initialize(f3,w,AssumptionSet())

    val sequence_builder = new SequenceBuilder(WTSStateNode(w,su,0),sol_builder)

    //sequence_builder.log_state

    //println("s0->A")
    sequence_builder.update_seq_with("s0","A")
    //println("A->B")
    sequence_builder.update_seq_with("A","B")

    //sequence_builder.log_state

    assertEquals( true , sequence_builder.partial.contains(StateSequence(ArrayBuffer("s0","A","B"),false,false)) )

    //println("A->C")
    sequence_builder.update_seq_with("A","C")

    //sequence_builder.log_state

    assertEquals( true , sequence_builder.partial.contains(StateSequence(ArrayBuffer("s0","A","B"),false,false)) )
    assertEquals( true , sequence_builder.partial.contains(StateSequence(ArrayBuffer("s0","A","C"),false,false)) )

    //println("B->D")
    sequence_builder.update_seq_with("B","D")
    //sequence_builder.log_state


    //println("s0->B")
    sequence_builder.update_seq_with("s0","B")
    //sequence_builder.log_state
    assertEquals( true , sequence_builder.partial.contains(StateSequence(ArrayBuffer("s0","A","C"),false,false)) )
    assertEquals( true , sequence_builder.partial.contains(StateSequence(ArrayBuffer("s0","A","B","D"),false,false)) )
    assertEquals( false , sequence_builder.partial.contains(StateSequence(ArrayBuffer("s0","B"),false,false)) )
    assertEquals( true , sequence_builder.partial.contains(StateSequence(ArrayBuffer("s0","B","D"),false,false)) )

    //println("D->A")
    sequence_builder.update_seq_with("D","A")
    //sequence_builder.log_state

    assertEquals( true , sequence_builder.partial.contains(StateSequence(ArrayBuffer("s0","A","B","D","A"),true,false)) )
    assertEquals( true , sequence_builder.partial.contains(StateSequence(ArrayBuffer("s0","B","D","A","B"),true,false)) )
    assertEquals( true , sequence_builder.partial.contains(StateSequence(ArrayBuffer("s0","B","D","A","C"),false,false)) )
    assertEquals( true , sequence_builder.partial.contains(StateSequence(ArrayBuffer("s0","A","C"),false,false)) )

    //println("D->s0")
    sequence_builder.update_seq_with("D","s0")
    //sequence_builder.log_state

    //println("D->E (exit)")
    sequence_builder.update_seq_with("D","E",true)
    //sequence_builder.log_state

  }

  def testUpdateSeqWithForward (): Unit = {
    val w = StateOfWorld.create(GroundPredicate("attach", AtomTerm("doc")))
    val f1 = Globally(LogicAtom("document", AtomTerm("doc")))
    val f2 = Finally(LogicAtom("ready", AtomTerm("doc")))
    val f3 = LogicConjunction(f1, f2)
    val builder = new SupervisorBuilder()
    val su = builder.initialize(f3, w, AssumptionSet())

    val sequence_builder = new SequenceBuilder(WTSStateNode(w, su, 0),sol_builder)


    sequence_builder.update_seq_with("s0", "B")
    sequence_builder.update_seq_with("B", "C")
    sequence_builder.update_seq_with("C", "D")

    sequence_builder.update_seq_with("s0", "A")

    sequence_builder.update_seq_with("A", "B")

    assertEquals( true , sequence_builder.partial.contains(StateSequence(ArrayBuffer("s0","B","C","D"),false,false)) )
    assertEquals( true , sequence_builder.partial.contains(StateSequence(ArrayBuffer("s0","A","B","C","D"),false,false)) )
  }

  def testUpdateMultiExpansion (): Unit = {
    val w0 = StateOfWorld.create(GroundPredicate("a", AtomTerm("a")))

    val f1 = Globally(LogicAtom("document", AtomTerm("doc")))
    val f2 = Finally(LogicAtom("ready", AtomTerm("doc")))
    val f3 = LogicConjunction(f1, f2)
    val builder = new SupervisorBuilder()
    val su = builder.initialize(f3, w0, AssumptionSet())
    val sequence_builder = new SequenceBuilder(WTSStateNode(w0, su, 0),sol_builder)

    sequence_builder.update_seq_with("s0", "A")
    val x1 = sequence_builder.add_xor(Array("scen1","scen2"))
    sequence_builder.update_seq_with("A", x1)
    //sequence_builder.update_seq_with(x1,"scen1")
    //sequence_builder.update_seq_with(x1,"scen2")
    sequence_builder.partial = sequence_builder.partial + StateSequence(ArrayBuffer[String]("x0.scen1"),false,false)
    sequence_builder.partial = sequence_builder.partial + StateSequence(ArrayBuffer[String]("x0.scen2"),false,false)
    sequence_builder.update_seq_with("x0.scen1", "B")
    sequence_builder.update_seq_with("x0.scen2", "C")

    //sequence_builder.log_state

  }

  def test_with_capabilities() : Unit = {
    val w0 = StateOfWorld.create(GroundPredicate("a", AtomTerm("doc")))
    val f = Globally(LogicAtom("a", AtomTerm("doc")))
    val builder = new SupervisorBuilder()
    val su = builder.initialize(f, w0, AssumptionSet())
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

    val expl = SingleGoalProblemExploration(ps, w0, cap_set)


    val sequence_builder = new SequenceBuilder(WTSStateNode(w0, su, 0),sol_builder)
    //sequence_builder.log_state

    expl.execute_iteration()

    val exp : SimpleWTSExpansion = expl.highest_expansion.get.asInstanceOf[SimpleWTSExpansion]

    //println(exp)

    sequence_builder.deal_with_expansion(exp)

    //sequence_builder.log_state

    expl.pick_expansion(exp)
    expl.new_node(exp.end)
    expl.execute_iteration()

    val exp2 : MultiWTSExpansion = expl.highest_expansion.get.asInstanceOf[MultiWTSExpansion]
    //println(exp2)

    sequence_builder.deal_with_multi_expansion(exp2)

    //sequence_builder.log_state

  }

  def test_simple_solution_from_capabilities() : Unit = {
    val w0 = StateOfWorld.create(GroundPredicate("a", AtomTerm("doc")))
    val f = Finally(LogicAtom("d", AtomTerm("doc")))
    val builder = new SupervisorBuilder()
    val su = builder.initialize(f, w0, AssumptionSet())
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

    val expl = SingleGoalProblemExploration(ps, w0, cap_set)
    val sequence_builder = new SequenceBuilder(WTSStateNode(w0, su, 0),sol_builder)
    //sequence_builder.log_state

    expl.execute_iteration()
    val exp : SimpleWTSExpansion = expl.highest_expansion.get.asInstanceOf[SimpleWTSExpansion]
    //println(exp)
    sequence_builder.deal_with_expansion(exp)
    //sequence_builder.log_state
    expl.pick_expansion(exp)
    expl.new_node(exp.end)

    expl.execute_iteration()
    val exp2 : SimpleWTSExpansion = expl.highest_expansion.get.asInstanceOf[SimpleWTSExpansion]
    //println(exp2)
    sequence_builder.deal_with_expansion(exp2)
    //sequence_builder.log_state
    expl.pick_expansion(exp2)
    expl.new_node(exp2.end)

    expl.execute_iteration()
    val exp3 : SimpleWTSExpansion = expl.highest_expansion.get.asInstanceOf[SimpleWTSExpansion]
    //println(exp2)
    sequence_builder.deal_with_expansion(exp3)
    //sequence_builder.log_state
    expl.pick_expansion(exp3)
    expl.new_node(exp3.end)



   for (s <- sequence_builder.complete) {
     val builder = new EarlyDecisionSolutionBuilder
     val sol = builder.solution_from_simple_sequence(s,SequenceInterpretation(null,sequence_builder.cap_map,null,null))
     /*if (sol.isDefined)
        sol.get.print_for_graphviz*/

   }
  }


  def test_partial_solution_with_capabilities() : Unit = {
    val w0 = StateOfWorld.create(GroundPredicate("a", AtomTerm("doc")))
    val f = Globally(LogicAtom("a", AtomTerm("doc")))
    val builder = new SupervisorBuilder()
    val su = builder.initialize(f, w0, AssumptionSet())
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

    val expl = SingleGoalProblemExploration(ps, w0, cap_set)

    val local_builder = new WTSLocalBuilder(ps,w0,cap_set,MaxEmptyIterationTermination(10),sol_builder)
    local_builder.build_wts()

    //local_builder.wts.print_for_graphviz(quality.pretty_string)

    for (p <- local_builder.sol_builder.partial) {
      val builder = new EarlyDecisionSolutionBuilder
      val sol = builder.solution_from_xor_sequence(p,SequenceInterpretation(null,local_builder.sol_builder.cap_map,local_builder.sol_builder.scenario_map, local_builder.sol_builder.decision_map))
      /*if (sol.isDefined)
        sol.get.print_for_graphviz*/
    }

  }


}
