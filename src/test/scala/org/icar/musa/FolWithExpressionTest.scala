package org.icar.musa

import junit.framework.TestCase
import net.sf.tweety.logics.fol.semantics.HerbrandInterpretation
import net.sf.tweety.logics.fol.syntax.FOLAtom
import net.sf.tweety.lp.asp.syntax.Program
import org.icar.fol.Entail.{solver, tx}
import org.icar.fol._
import org.icar.musa.context.StateOfWorld

import scala.collection.mutable.ArrayBuffer

class FolWithExpressionTest extends TestCase {

  def testAssumptionWithExpression (): Unit = {
    val a = Assumption("anomaly(user, temperature) :- not temperature(user, normal).")
    val b = Assumption("anomaly(user, temperature) :- temperature(user, T), T>37.")
    val c = Assumption("anomaly(user, temperature, T) :- temperature(user, T), T>37.")
  }

  def testEntailWithExpression (): Unit = {
    val a = Assumption("anomaly(user, heart_rate) :- not heart_rate(user, normal).")
    val b = Assumption("ill(user) :- anomaly(user, _ ).")
    val c = Assumption("anomaly(user, temperature) :- temperature(user, T), T>37.")

    val assertionset = AssumptionSet(a,b,c)
    val base = new Program(assertionset.as_list)



    val w = StateOfWorld.create(  GroundPredicate("temperature", AtomTerm("user"), NumeralTerm(38))  )
    println(w)

    val entail = Entail

    for (s <- w.statements) {
      //println("Adding: "+s.toString)
      //base.addFact(entail.rule_for_asl(s))
    }

    println("Base:"+base.toStringFlat)

    val response = solver.computeModels(base, 100)
    if (response != null) {
      val as = response.get(0)
      println(as.toString)

      val interpr = new HerbrandInterpretation()

      val it = as.iterator()
      while (it.hasNext) {
        val f = tx.toFOL(it.next())
        interpr.add(f.asInstanceOf[FOLAtom])
      }

      //val reply = interpr satisfies TweetyFormula.fromCond(c)
    }
  }

}
