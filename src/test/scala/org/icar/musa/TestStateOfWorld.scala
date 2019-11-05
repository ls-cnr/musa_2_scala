package org.icar.musa

import junit.framework.TestCase
import org.icar.fol._
import org.icar.musa.context.{Deprec_AddEvoOperator, Deprec_RemoveAllEvoOperator, Deprec_RemoveEvoOperator, StateOfWorld}


class TestStateOfWorld extends TestCase {

    val entail = Entail

    def testEntail (): Unit = {
        val luca = GroundPredicate("name", AtomTerm("luca"))
        val emilio = GroundPredicate("name", AtomTerm("emilio"))
        val ale = GroundPredicate("name", AtomTerm("ale"))
        val giusi = GroundPredicate("name", AtomTerm("giusi"))
        val simo = GroundPredicate("name", AtomTerm("simo"))
        val luigi = GroundPredicate("surname", AtomTerm("luigi"))

        val w = StateOfWorld.create(luca, emilio, ale)
        val w1 = StateOfWorld.create(luca, ale, emilio)
        val w2 = StateOfWorld.extend(w1, giusi, simo)

        val w3 = StateOfWorld.extend(w1, Deprec_AddEvoOperator(giusi))
        val w4 = StateOfWorld.extend(w1, Deprec_RemoveEvoOperator(luca))
        val w5 = StateOfWorld.extend(w4, Deprec_AddEvoOperator(luca))
        val w6 = StateOfWorld.extend(w5, Deprec_AddEvoOperator(luigi))
        val w7 = StateOfWorld.extend(w6, Deprec_RemoveAllEvoOperator("name"))

        assert(w == w1)
        assert(!(w1 == w2))
        assert(w1 == w5)

        /* example of entail */
        val ass_set = AssumptionSet(Assumption("document(X) :- attach(X)."))
        val a = GroundPredicate("attach", AtomTerm("doc1"))
        val w8 = StateOfWorld.create(a)
        val c = FOLCondition(Literal(Predicate("document", AtomTerm("doc1"))))
        val b = entail.condition(w8, ass_set, c)
        assert(b)
    }
}
  
