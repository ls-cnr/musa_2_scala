package org.icar.musa.scenarios

import net.sf.tweety.lp.asp.parser.ASPParser
import org.icar.fol.{Assumption, AssumptionSet, AtomTerm, GroundPredicate}
import org.icar.ltl._
import org.icar.musa.context.StateOfWorld
import org.icar.musa.pmr._
import org.icar.musa.spec.{AbstractCapabilityParser, AbstractCapability, LTLGoal}

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

class EntertainmentScenario extends Scenario {

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
    val url = getClass.getResource("PRIN_capabilities.cap")
    println("recovering file: "+url.getFile)
    val s = Source.fromFile(url.getFile)
    val parser = new AbstractCapabilityParser()
    //parser.parseAll()
    val p = parser.parseAll(parser.cap_specification,s.mkString)

    p.get.toArray
  }

  override def termination: TerminationDescription = IterationTermination(50) //MaxEmptyIterationTermination(5)// OrTermination(ArrayBuffer[TerminationDescription](IterationTermination(50),MaxEmptyIterationTermination(5)))
}
