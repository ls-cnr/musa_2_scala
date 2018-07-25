package org.icar.musa

import junit.framework.TestCase
import junit.framework.Assert.assertEquals
import org.icar.fol._
import org.icar.ltl.supervisor.{NetHierarchy, NetSupervisor}
import org.icar.ltl._
import org.icar.musa.context.{AddEvoOperator, EvoOperator, RemoveEvoOperator, StateOfWorld}
import org.icar.petrinet.{AcceptedState, ErrorState, WaitErrorState}

import scala.collection.mutable.ArrayBuffer

class SupervisorTest extends TestCase {

  def testLTLFormula (): Unit = {
    val f1 = Globally(LogicAtom("document",AtomTerm("doc")))
    assertEquals("Globally(LogicAtom(document(doc)))",f1.toString)
  }

  def testNetHierarchy (): Unit = {
    val f1 = Globally(LogicAtom("document",AtomTerm("doc")))
    val f2 = Finally(LogicAtom("ready",AtomTerm("doc")))
    val f3 = LogicConjunction(f1,f2)

    val nh = NetHierarchy(f3)
  }

  def testPetrinet (): Unit = {
    val su = init_net_supervisor_1

    assertEquals("start", su.petrinets("G2").get_places_with_tokens.head.id)
    assertEquals (1, su.petrinets("G2").get_fireable_transitions.size)
    assertEquals (WaitErrorState, su.current_state)

    su.petrinets("G2").fire(su.petrinets("G2").get_fireable_transitions.head)
    assertEquals("end", su.petrinets("G2").get_places_with_tokens.head.id)
    assertEquals (0, su.petrinets("G2").get_fireable_transitions.size)

    assertEquals("start", su.petrinets("F4").get_places_with_tokens.head.id)
  }

  def init_net_supervisor_1 : NetSupervisor = {
    val ass = AssumptionSet( Assumption("document(X) :- attach(X).") )
    val a = GroundPredicate("attach",AtomTerm("doc"))
    val w = StateOfWorld.create(a)


    val f1 = Globally(LogicAtom("document",AtomTerm("doc")))
    val f2 = Finally(LogicAtom("ready",AtomTerm("doc")))
    val f3 = LogicConjunction(f1,f2)

    NetSupervisor.initialize(f3,w,ass)
  }


  def testSupervisor_Finally_and_Globally(): Unit = {
    val ass = AssumptionSet( Assumption("document(X) :- attach(X).")  )

    val w = StateOfWorld.create(GroundPredicate("attach",AtomTerm("doc")))
    val w2 = StateOfWorld.extend(w,GroundPredicate("ready",AtomTerm("doc")))

    val f1 = Globally(LogicAtom("document",AtomTerm("doc")))
    val f2 = Finally(LogicAtom("ready",AtomTerm("doc")))
    val f3 = LogicConjunction(f1,f2)

    val su = NetSupervisor.initialize(f3,w,ass)

    val su2 = NetSupervisor.update(su,w2,ass)

    assertEquals("start", su2.petrinets("G2").get_places_with_tokens.head.id)
    assertEquals("end", su2.petrinets("F4").get_places_with_tokens.head.id)
    assertEquals(true, su2.isAccepted)

  }


  def testSupervisor_Until(): Unit = {
    val ass = AssumptionSet(  )

    val w = StateOfWorld.create(GroundPredicate("attach",AtomTerm("doc")))
    val w2 = StateOfWorld.extend(w,GroundPredicate("ready",AtomTerm("doc")))

    val op1: EvoOperator = AddEvoOperator(GroundPredicate("available", AtomTerm("doc")))
    val op2: EvoOperator = RemoveEvoOperator(GroundPredicate("attach", AtomTerm("doc")))
    val w3 = StateOfWorld.extend(w2, Array(op1,op2))

    val f = Until(LogicAtom("attach",AtomTerm("doc")), LogicAtom("available",AtomTerm("doc")))

    val su = NetSupervisor.initialize(f,w,ass)
    assertEquals("start", su.petrinets("U1").get_places_with_tokens.head.id)
    assertEquals(WaitErrorState, su.current_state)

    val su2 = NetSupervisor.update(su,w2,ass)
    assertEquals("start", su2.petrinets("U1").get_places_with_tokens.head.id)
    assertEquals(WaitErrorState, su2.current_state)

    val su3 = NetSupervisor.update(su2,w3,ass)
    assertEquals("e1", su3.petrinets("U1").get_places_with_tokens.head.id)
    assertEquals(AcceptedState, su3.current_state)
  }

  def testSupervisor_Next(): Unit = {
    val ass = AssumptionSet(  )

    val w = StateOfWorld.create(GroundPredicate("attach",AtomTerm("doc")))
    val w2 = StateOfWorld.extend(w,GroundPredicate("ready",AtomTerm("doc")))
    val w3 = StateOfWorld.extend(w,GroundPredicate("notready",AtomTerm("doc")))

    val f = Next(LogicAtom(GroundPredicate("ready",AtomTerm("doc"))))

    val su = NetSupervisor.initialize(f,w,ass)
    assertEquals(WaitErrorState, su.current_state)

    val su_a = NetSupervisor.update(su,w2,ass)
    assertEquals(AcceptedState, su_a.current_state)

    val su_b = NetSupervisor.update(su,w3,ass)
    assertEquals(ErrorState, su_b.current_state)
  }


  def testSupervisor_Imply(): Unit = {
    val ass = AssumptionSet(  )

    val w = StateOfWorld.create(GroundPredicate("document",AtomTerm("doc")))
    val w2 = StateOfWorld.extend(w,GroundPredicate("ready",AtomTerm("doc")))
    val w3 = StateOfWorld.extend(w2,GroundPredicate("notworked",AtomTerm("doc")))
    val w4 = StateOfWorld.extend(w3,GroundPredicate("worked",AtomTerm("doc")))

    val f1 = LogicAtom("ready",AtomTerm("doc"))
    val f2 = Finally(LogicAtom("worked",AtomTerm("doc")))

    val f = LogicImplication(f1,f2)

    val su = NetSupervisor.initialize(f,w,ass)
    assertEquals(AcceptedState, su.current_state)

    val su1 = NetSupervisor.update(su,w2,ass)
    assertEquals(WaitErrorState, su1.current_state)

    val su2 = NetSupervisor.update(su1,w3,ass)
    assertEquals(WaitErrorState, su2.current_state)

    val su3 = NetSupervisor.update(su2,w4,ass)
    assertEquals(AcceptedState, su3.current_state)
  }


}
