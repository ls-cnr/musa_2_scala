package org.icar.pmr_solver

import net.sf.tweety.logics.fol.syntax.{FolFormula => TweetyF}
import org.icar.fol.{TweetyFormula, HighLevel_PredicateFormula}
import org.icar.ltl.{LogicAtom, LogicBiImplication, LogicConjunction, LogicDisjunction, LogicFalse, LogicImplication, LogicTrue, ltlFormula, Finally => ltlFinally, Globally => ltlGlobally, Next => ltlNext, Release => ltlRelease, Until => ltlUntil}
import org.icar.musa.context.EvoOperator
import org.icar.musa.main_entity.{EvolutionScenario, GroundedAbstractCapability}

import scala.collection.mutable


/* Associate each action to the corresponsing Capability/Perturbation by giving a value to the params */
case class CapActionBinding(action: SystemAction, capability: Capability, grounding: ParamsGrounding)
case class PertActionBinding(action: EnvironmentAction, perturbation: Perturbation, grounding: ParamsGrounding)

case class ParamsGrounding(setting : Array[Grounding])
case class Grounding(variable : String, value : String)


/* operate the association */
object MUSA_entities {

  def ltlFormula_to_LTLformula(ltl : ltlFormula) : HighLevel_LTLformula = {
    ltl match {
      case ltlGlobally(f) => Globally(ltlFormula_to_LTLformula(f))
      case ltlFinally(f) => Finally(ltlFormula_to_LTLformula(f))
      case ltlNext(f) => Next(ltlFormula_to_LTLformula(f))
      case ltlUntil(f1,f2) => Until(ltlFormula_to_LTLformula(f1),ltlFormula_to_LTLformula(f2))
      case ltlRelease(f1,f2) => Release(ltlFormula_to_LTLformula(f1),ltlFormula_to_LTLformula(f2))

      case LogicAtom(p) => Predicate(p)
      case LogicTrue() => True()
      case LogicFalse() => False()

      case LogicImplication(f1,f2) => Implication(ltlFormula_to_LTLformula(f1),ltlFormula_to_LTLformula(f2))
      case LogicBiImplication(f1,f2) => BiImplication(ltlFormula_to_LTLformula(f1),ltlFormula_to_LTLformula(f2))
      case LogicConjunction(formulas) =>
        val list = formulas.toList
        if(list.isEmpty)
          True()
        else if (list.size==2)
          Conjunction(
            ltlFormula_to_LTLformula(list.head),
            ltlFormula_to_LTLformula(list.tail.head)
          )
        else
            Conjunction(
              ltlFormula_to_LTLformula(list.head),
              ltlFormula_to_LTLformula(LogicConjunction(list.tail.to[mutable.ArrayBuffer])))
      case LogicDisjunction(formulas) =>
        val list = formulas.toList
        if(list.isEmpty)
          False()
        else if (list.size==2)
          Disjunction(
            ltlFormula_to_LTLformula(list.head),
            ltlFormula_to_LTLformula(list.tail.head)
          )
        else
          Disjunction(
            ltlFormula_to_LTLformula(list.head),
            ltlFormula_to_LTLformula(LogicDisjunction(list.tail.to[mutable.ArrayBuffer])))
      case _ => Empty()
    }

  }

  def capability_to_system_actions(cap:GroundedAbstractCapability):List[SystemAction] = {
    // by now NOT considering params => 1 cap = 1 system_action

    //val pre = convert_folFormula_to_TweetyF(cap.pre.formula)
    val effects = convert_scenarios_to_effects(cap.scenarios)

    List(SystemAction(cap.name,List.empty,cap.pre.formula,effects))
  }

  //def perturbation_to_environment_actions = ???


  def convert_folFormula_to_TweetyF(pre:HighLevel_PredicateFormula) : TweetyF = TweetyFormula.fromFormula(pre)


  def convert_scenarios_to_effects(scenario:Map[String,EvolutionScenario]): Array[EvolutionGrounding] = {
    var list : List[EvolutionGrounding] = List.empty
    for (s<-scenario) {
      val name = s._1
      val evolution: Array[EvoOperator] = s._2.evo

      list = EvolutionGrounding(name, evolution) :: list
    }

    list.toArray
  }



}

/* these should be defined outside */
class Capability {

}

class Perturbation {

}











