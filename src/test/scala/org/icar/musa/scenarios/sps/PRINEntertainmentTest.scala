package org.icar.musa.scenarios.sps

import junit.framework.TestCase
import org.icar.fol.{Assumption, AssumptionSet, AtomTerm, GroundPredicate}
import org.icar.ltl._
import org.icar.musa.context.StateOfWorld
import org.icar.musa.pmr._
import org.icar.musa.scenarios.TestScenario
import org.icar.musa.spec.{AbstractCapability, LTLGoal}
import org.icar.musa.spec.TestLTLParser.{cap_specification, parseAll}

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

class PRINEntertainmentTest extends TestCase with TestScenario {

  def testDomain (): Unit = {
    val wtsbuilder = new WTSLocalBuilder(problem,initial_state,capabilities,termination)
    wtsbuilder.build_wts()

    println("TERMINATED")

    for (comp <- wtsbuilder.sol_builder.complete)
      println(comp)

    wtsbuilder.wts.print_for_graphviz(problem.asset.pretty_string)

    for (sol <- wtsbuilder.sol_builder.complete_solution)
      sol.print_for_graphviz()

  }

  override def problem: SingleGoalProblemSpecification = {
    val ass_set = assumption_set
    val goal = goal_specification
    val asset = quality_asset
    SingleGoalProblemSpecification(ass_set,goal_specification,asset)
  }

  override def assumption_set  : AssumptionSet= {
    val list = ArrayBuffer[Assumption]()
    list += Assumption("anomaly(user, temperature) :- not temperature(user, normal).")
    list += Assumption("anomaly(user, heart_rate) :- not heart_rate(user, normal).")
    list += Assumption("anomaly(user, pressure) :- not pressure(user, normal).")
    list += Assumption("anomaly(user, danger) :- not location(user, bedroom), posture(user, laying).")
    list += Assumption("ill(user) :- anomaly(user, _ ).")
    list += Assumption("sleeping(user) :- posture(user, laying), location(user, bedroom).")

    AssumptionSet(list: _*)
  }

  override def goal_specification: LTLGoal =
    LTLGoal(LogicImplication(
      LogicAtom(GroundPredicate("entertainment_time",AtomTerm("user"))),
      Finally(
        LogicDisjunction(
          LogicAtom(GroundPredicate("passed_entertainment_time",AtomTerm("user"))),
          LogicAtom(GroundPredicate("alert_thrown",AtomTerm("user")))
        )
      )
    ))

  override def quality_asset: QualityAsset = new EmptyQualityAsset(AssumptionSet())

  override def initial_state : StateOfWorld = StateOfWorld.create(
    GroundPredicate("entertainment_time", AtomTerm("user")),
    GroundPredicate("temperature", AtomTerm("user"),AtomTerm("normal")),
    GroundPredicate("heart_rate", AtomTerm("user"),AtomTerm("normal")),
    GroundPredicate("pressure", AtomTerm("user"),AtomTerm("normal")),
    GroundPredicate("location", AtomTerm("user"),AtomTerm("living_room")),
    GroundPredicate("posture", AtomTerm("user"),AtomTerm("standing")),
    GroundPredicate("openness", AtomTerm("user"),AtomTerm("high"))
  )

  override def capabilities: Array[AbstractCapability] = {
    val file = "./src/test/scala/org/icar/musa/scenarios/PRIN_capabilities"
    val s = Source.fromFile(file)
    val p = parseAll(cap_specification,s.mkString)

    p.get.toArray
  }

  override def termination: TerminationDescription = MaxEmptyIterationTermination(5)
}
