package org.icar.musa.scenarios

import junit.framework.TestCase
import org.icar.fol._
import org.icar.ltl._
import org.icar.musa.context.{AddEvoOperator, EvoOperator, RemoveEvoOperator, StateOfWorld}
import org.icar.musa.pmr._
import org.icar.musa.spec.TestLTLParser.{cap_specification, parseAll}
import org.icar.musa.spec.{AbstractCapability, EvolutionScenario, GroundedAbstractCapability, LTLGoal}

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

class PRINTest extends TestCase with TestScenario {

  def testDomain (): Unit = {
    val wtsbuilder = new WTSLocalBuilder(problem,initial_state,capabilities,termination)
    wtsbuilder.build_wts()

    println("TERMINATED")

    for (comp <- wtsbuilder.sol_builder.complete)
      println(comp)

    //wtsbuilder.sol_builder.log_mapping()

    //wtsbuilder.wts.print_for_graphviz(wtsbuilder.sol_builder.pretty_print)
    wtsbuilder.wts.print_for_graphviz(problem.asset.pretty_string)

    for (sol <- wtsbuilder.sol_builder.complete_solution)
      sol.print_for_graphviz()

    //sol.print_for_graphviz()
    /*for (seq <- wtsbuilder.sol_builder.partial) {
      val sol = Solution.build_from_xor_sequence(seq,wtsbuilder.sol_builder.cap_map, wtsbuilder.sol_builder.scenario_map)
      if (sol.isDefined)
        sol.get.print_for_graphviz()
    }*/
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

  override def goal_specification : LTLGoal =
    LTLGoal(LogicImplication(
      LogicAtom(GroundPredicate("wake_up_time",AtomTerm("user"))),
      Finally(
        LogicDisjunction(
          LogicAtom(GroundPredicate("standing",AtomTerm("user"))),
          LogicAtom(GroundPredicate("alert_thrown",AtomTerm("user")))
        )
      )
    )) /* wake_up_time(user) -> F ( standing(user) or alert_thrown(user) ) */

  override def quality_asset : QualityAsset = new EmptyQualityAsset(AssumptionSet())

  override def initial_state : StateOfWorld = StateOfWorld.create(
    GroundPredicate("wake_up_time", AtomTerm("user")),
    GroundPredicate("temperature", AtomTerm("user"),AtomTerm("normal")),
    GroundPredicate("heart_rate", AtomTerm("user"),AtomTerm("normal")),
    GroundPredicate("pressure", AtomTerm("user"),AtomTerm("normal")),
    GroundPredicate("location", AtomTerm("user"),AtomTerm("bedroom")),
    GroundPredicate("posture", AtomTerm("user"),AtomTerm("laying"))
  )



  override def capabilities : Array[AbstractCapability] = {
    val file = "./src/test/scala/org/icar/musa/scenarios/PRIN_capabilities"
    val s = Source.fromFile(file)
    val p = parseAll(cap_specification,s.mkString)

    p.get.toArray
    //Array[AbstractCapability](check_wake_up,remind_wake_up,alert_anomaly)
  }

  private def check_wake_up : AbstractCapability = {
    val pre = FOLCondition(Conjunction(
      Literal(Predicate("sleeping", AtomTerm("user"))),
      Negation(Literal(Predicate("ill", AtomTerm("user")))),
      Disjunction(
        Conjunction(Literal(Predicate("wake_up_time", AtomTerm("user"))),Negation(Literal(Predicate("passed_wake_up_time", AtomTerm("user"))))),
        Literal(Predicate("waiting_after_remind", AtomTerm("user"))) )
    ))
    val post = FOLCondition(
      Disjunction( Literal(Predicate("standing", AtomTerm("user"))), Literal(Predicate("anomaly", AtomTerm("user"))))
    )

    val evo_1 = ( "standing" -> EvolutionScenario(Array[EvoOperator](
        RemoveEvoOperator(GroundPredicate("posture", AtomTerm("user"), AtomTerm("laying"))),
        AddEvoOperator(GroundPredicate("standing", AtomTerm("user")))
      )
    ))

    //val evo_2 = ( "sleeping" -> EvolutionScenario(Array[EvoOperator](AddEvoOperator(GroundPredicate("still_sleeping", AtomTerm("user")))) ))

    val evo_3 = ( "over_sleeping" -> EvolutionScenario(Array[EvoOperator](
        //RemoveEvoOperator(GroundPredicate("wake_up_time", AtomTerm("user"))),
        AddEvoOperator(GroundPredicate("passed_wake_up_time", AtomTerm("user")))
      )
    ))

    val evo_4 = ( "anomaly" -> EvolutionScenario(Array[EvoOperator](
        AddEvoOperator(GroundPredicate("ill", AtomTerm("user")))
      )
    ))

    GroundedAbstractCapability("check_wake_up",pre,post,Map(evo_1,evo_3,evo_4))
  }

  private def remind_wake_up : AbstractCapability = {
    val pre = FOLCondition(Conjunction(
      Literal(Predicate("sleeping", AtomTerm("user"))),
      Negation(Literal(Predicate("ill", AtomTerm("user")))),
      Literal(Predicate("passed_wake_up_time", AtomTerm("user"))),
      Negation(Literal(Predicate("waiting_after_remind", AtomTerm("user"))))
    ))
    val post = FOLCondition(
      Literal(Predicate("waiting_after_remind", AtomTerm("user")))
    )

    val evo_1 = ( "remind" -> EvolutionScenario(Array[EvoOperator](
         AddEvoOperator(GroundPredicate("waiting_after_remind", AtomTerm("user")))
      )
    ))

    GroundedAbstractCapability("remind_wake_up",pre,post,Map(evo_1))
  }

  private def alert_anomaly : AbstractCapability = {
    val pre = FOLCondition(
      Literal(Predicate("ill", AtomTerm("user")))
    )
    val post = FOLCondition(
      Literal(Predicate("alert_thrown", AtomTerm("user")))
    )

    val evo_1 = ( "alert" -> EvolutionScenario(Array[EvoOperator](
      AddEvoOperator(GroundPredicate("alert_thrown", AtomTerm("user")))
    )
    ))

    GroundedAbstractCapability("alert_anomaly",pre,post,Map(evo_1))
  }


  override def termination: TerminationDescription = MaxEmptyIterationTermination(5)
}
