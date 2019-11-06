package org.icar.pmr_solver

import java.util

import net.sf.tweety.lp.asp.syntax.{Rule => TweetyRule}
import net.sf.tweety.logics.fol.syntax.{FolFormula => TweetyF}
import org.icar.fol.{Assumption, AtomTerm, ConstantTerm, GroundPredicate, HighLevel_PredicateFormula, IntegerTerm, StringTerm, Term}
import org.icar.musa.context.{EvoOperator, StateOfWorld}




/******* PLANNING DOMAIN ********/
case class Domain (predicates : Array[DomainPredicate], types: Array[DomainType], axioms : Array[axiom], qos : RawState => Float) {

  /* ro remove
  def axioms_as_rulelist: util.ArrayList[TweetyRule] = {
    val list = new util.ArrayList[TweetyRule]()
    for (a <- axioms)
      list.add(a.rule)
    list
  }*/

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
  def range(types: Array[DomainType]) : List[ConstantTerm]
}
case class DomainVariable(name:String, category : String) extends DomainPredArguments {
  override def range(types: Array[DomainType]): List[ConstantTerm] =   {
    val tpe = types.find(_.name == category)
    if (tpe.isDefined)
      tpe.get.range
    else
      List.empty

  }
}
case class DomainConstant(name : String) extends DomainPredArguments {
  override def range(types: Array[DomainType]): List[ConstantTerm] = List(AtomTerm(name))
}
case class DomainConstantString(str : String) extends DomainPredArguments {
  override def range(types: Array[DomainType]): List[ConstantTerm] = List(StringTerm(str))
}
case class NullDomainType() extends DomainPredArguments {
  override def range(types: Array[DomainType]) : List[ConstantTerm] = List.empty
}



abstract class DomainType(val name:String) {
  def range : List[ConstantTerm]
}
case class NumericDomainType(override val name:String, min : Int, max : Int) extends DomainType(name) {
  override def range: List[ConstantTerm] = {
    val numeric_range = (min to max).toList
    for (n <- numeric_range) yield IntegerTerm(n)
  }
}
case class EnumerativeDomainType(override val name:String,enumer : Array[String]) extends DomainType(name) {
  override def range: List[ConstantTerm] = {
    val array = for (e<-enumer) yield AtomTerm(e)
    array.toList
  }
}





/******* PLANNING PROBLEM ********/
case class Problem(val I : StateOfWorld, val goal_model : LTLGoalSet, val actions : AvailableActions)

case class LTLGoalSet(goals:Array[HighLevel_LTLformula])

/*
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
*/

/******* LTL SYNTAX DEFINITION ********/
sealed abstract class HighLevel_LTLformula

case class Predicate(p:GroundPredicate) extends HighLevel_LTLformula
case class True() extends HighLevel_LTLformula
case class False() extends HighLevel_LTLformula
case class Empty() extends HighLevel_LTLformula

case class Implication(left : HighLevel_LTLformula, right : HighLevel_LTLformula) extends HighLevel_LTLformula
case class BiImplication(left : HighLevel_LTLformula, right : HighLevel_LTLformula) extends HighLevel_LTLformula
case class Negation(formula : HighLevel_LTLformula) extends HighLevel_LTLformula
case class Conjunction(left : HighLevel_LTLformula, right : HighLevel_LTLformula) extends HighLevel_LTLformula
case class Disjunction(left : HighLevel_LTLformula, right : HighLevel_LTLformula) extends HighLevel_LTLformula

case class Globally(formula : HighLevel_LTLformula) extends HighLevel_LTLformula
case class Finally(formula : HighLevel_LTLformula) extends HighLevel_LTLformula
case class Next(formula : HighLevel_LTLformula) extends HighLevel_LTLformula
case class Until(left : HighLevel_LTLformula, right : HighLevel_LTLformula) extends HighLevel_LTLformula
case class Release(left : HighLevel_LTLformula, right : HighLevel_LTLformula) extends HighLevel_LTLformula






/******* PLANNING ACTIONS ********/
case class AvailableActions(sys_action : Array[SystemAction], env_action : Array[EnvironmentAction])


abstract class PlanningAction

case class SystemAction(
                           id : String,
                           params: List[DomainPredArguments],
                           pre : HighLevel_PredicateFormula,
                           post : HighLevel_PredicateFormula,
                           effects : Array[EvolutionGrounding]
           ) extends PlanningAction

case class EnvironmentAction(
                                id : String,
                                params: List[DomainPredArguments],
                                pre : HighLevel_PredicateFormula,
                                post : HighLevel_PredicateFormula,
                                effects : Array[ProbabilisticEvolutionGrounding]
            ) extends PlanningAction


case class EvolutionGrounding(name : String, evo : Array[EvoOperator])
case class ProbabilisticEvolutionGrounding(name : String, probability : Float, evo : Array[EvoOperator])











