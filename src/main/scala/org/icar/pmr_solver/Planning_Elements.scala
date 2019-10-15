package org.icar.pmr_solver

import java.util

import net.sf.tweety.lp.asp.syntax.{Rule => TweetyRule}
import net.sf.tweety.logics.fol.syntax.{FOLAtom, FolFormula => TweetyF}
import org.icar.fol.Assumption
import org.icar.musa.context.{EvoOperator, StateOfWorld}




/******* PLANNING DOMAIN ********/
case class Domain (types : Array[DomainType], atoms : Array[DomainConst], axioms : Array[Assumption], qos : Node => Float) {

  def axioms_as_rulelist: util.ArrayList[TweetyRule] = {
    val list = new util.ArrayList[TweetyRule]()
    for (a <- axioms)
      list.add(a.rule)
    list
  }

}


abstract class DomainType(name : String)
case class NumericDomainType(name : String, min : Integer, max : Integer) extends DomainType(name)
case class EnumerativeDomainType(name : String, range : Array[String])

case class DomainConst(name : String)





/******* PLANNING PROBLEM ********/
case class Problem(val I : StateOfWorld, val goal_model : LTLGoalSet, val actions : AvailableActions)



/*
abstract class GoalModel {
  def getSupervisor : GoalSupervisor
}
abstract class GoalSupervisor {
  def next(formula : LTLformula, check : (FOLAtom)=>Boolean) : GoalCheckMarker
}
*/

case class LTLGoalSet(goals:Array[LTLformula]) {
  def getSupervisors(s:Node) : Array[GoalSupervisor] = {
    for (g<-goals) yield new GoalSupervisor(s,g)
  }
}



/******* PLANNING ACTIONS ********/
case class AvailableActions(sys_action : Array[SystemAction], env_action : Array[EnvironmentAction])


abstract class PlanningAction

case class SystemAction(
             id : String,
             pre : TweetyF,
             effects : Array[EvolutionGrounding]
           ) extends PlanningAction

case class EnvironmentAction(
                         id : String,
                         pre : TweetyF,
                         effects : Array[ProbabilisticEvolutionGrounding]
            ) extends PlanningAction


case class EvolutionGrounding(name : String, evo : Array[EvoOperator])
case class ProbabilisticEvolutionGrounding(name : String, probability : Float, evo : Array[EvoOperator])






