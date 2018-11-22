package org.icar.musa.scenarios

import java.io.{InputStream, InputStreamReader}
import java.nio.charset.StandardCharsets
import java.util.Scanner

import org.icar.fol._
import org.icar.ltl._
import org.icar.musa.context.{AddEvoOperator, EvoOperator, RemoveEvoOperator, StateOfWorld}
import org.icar.musa.pmr._
import org.icar.musa.spec._

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

class WakeUpScenario(path:String = "org/icar/musa/scenarios") extends Scenario {

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


  /*override def capabilities: Array[AbstractCapability] = {
    val in : InputStream = this.getClass.getClassLoader.getResourceAsStream("./org/icar/musa/scenarios/PRIN_capabilities.cap")

    val reader = new InputStreamReader(in)
    //val str = convert(in,StandardCharsets.UTF_8)
    val parser = new ACParser()
    val p = parser.parseAll(parser.cap_specification,reader)//s.mkString)

    p.get.toArray
  }*/


/*  import java.io.IOException
  import java.nio.charset.Charset

  @throws[IOException]
  def convert(inputStream: InputStream, charset: Charset): String = {
    val scanner = new Scanner(inputStream, charset.name)
    try
      scanner.useDelimiter("\\A").next
    finally if (scanner != null) scanner.close()
  }*/


  override def capabilities : Array[AbstractCapability] = {
    val file = path+"/PRIN_capabilities.cap"
    val s = Source.fromFile(file)
    val parser = new ACParser()
    val p = parser.parseAll(parser.cap_specification,s.mkString)

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
