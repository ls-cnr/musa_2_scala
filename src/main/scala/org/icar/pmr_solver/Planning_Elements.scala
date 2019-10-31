package org.icar.pmr_solver

import java.util

import net.sf.tweety.lp.asp.syntax.{Rule => TweetyRule}
import net.sf.tweety.logics.fol.syntax.{FolFormula => TweetyF}
import org.icar.fol.Assumption
import org.icar.musa.context.{EvoOperator, StateOfWorld}




/******* PLANNING DOMAIN ********/
case class Domain (predicates : Array[DomainPredicate], axioms : Array[Assumption], qos : Node => Float) {

  def axioms_as_rulelist: util.ArrayList[TweetyRule] = {
    val list = new util.ArrayList[TweetyRule]()
    for (a <- axioms)
      list.add(a.rule)
    list
  }

  def get_predicate_arg_type(functional:String,pos:Int) : DomainPredArguments = {
    var t:DomainPredArguments=NullDomainType()
    for (p<-predicates)
      if (p.functor==functional && p.args.isDefinedAt(pos))
        t = p.args(pos)
    t
  }

}


case class DomainPredicate(functor : String, args : List[DomainPredArguments])


abstract class DomainPredArguments {
  def range : List[String]
}
case class DomainVariable(name:String, category : DomainType) extends DomainPredArguments {
  override def range: List[String] = category.range
}
case class DomainConstant(name : String) extends DomainPredArguments {
  override def range: List[String] = List(name)
}
case class NullDomainType() extends DomainPredArguments {
  override def range: List[String] = List.empty
}



abstract class DomainType() {
  def range : List[String]
}
case class NumericDomainType(min : Int, max : Int) extends DomainType() {
  override def range: List[String] = {
    val numeric_range = (min to max).toList
    for (n <- numeric_range) yield n.toString
  }
}
case class EnumerativeDomainType(enumer : Array[String]) extends DomainType() {
  override def range: List[String] = enumer.toList
}





/******* PLANNING PROBLEM ********/
case class Problem(val I : StateOfWorld, val goal_model : LTLGoalSet, val actions : AvailableActions)

case class LTLGoalSet(goals:Array[LTLformula]) {
  def getSupervisors(s:Node) : Array[GoalSupervisor] = {
    for (g<-goals) yield new GoalSupervisor(s,g)
  }


}

object LTLGoalSet {
  /*
 * This function checks if a particular node fully satisfies the set of goals
 *
 */
  def check_exit_node(sups : Array[GoalSupervisor]) : Boolean = {
    var exit=true
    for (s <- sups)
      if (!s.isFullSatisfied)
        exit = false

    exit
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






