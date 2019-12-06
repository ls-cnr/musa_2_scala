package org.icar.pmr_solver.high_level_specification

/******* PLANNING PROBLEM ********/
case class Problem(I : StateOfWorld, goal_model : LTLGoalSet, actions : AvailableActions)


case class StateOfWorld (statements : List[GroundPredicate]) {
    override def toString: String = {
        var a_string: String = ""
        a_string += "["
        for (i <- statements.indices) {
            a_string += statements(i).toString
            if (i<statements.length-1)
                a_string += ","
        }
        a_string + "]"
    }
}
//case class EmptyState() extends StateOfWorld(List.empty)


/******* GOAL SET: LTL SYNTAX DEFINITION ********/
case class LTLGoalSet(goals:Array[HL_LTLFormula])



/******* PLANNING ACTIONS ********/
case class AvailableActions(sys_action : Array[AbstractCapability], env_action : Array[AbstractCapability])


//abstract class PlanningAction

case class AbstractCapability (
                                  id : String,
                                  params: List[DomainArgument],
                                  //constraints : List[DomainVariableConstraint],
                                  pre : HL_PredicateFormula,
                                  post : HL_PredicateFormula,
                                  effects : Array[EvolutionGrounding],
                                  future : List[HL_PredicateFormula]
           )
object AbstractCapability {
    def empty(name:String) : AbstractCapability = AbstractCapability(name,List.empty,True(),True(),Array(),List.empty)

}
case class EvolutionGrounding(name : String, evo : Array[EvoOperator])
case class ProbabilisticEvolutionGrounding(
                                              name : String,
                                              probability : Float,
                                              evo : Array[EvoOperator]
                                          )

case class CapGrounding(c: AbstractCapability, ground: Map[String, ConstantTerm]) {
    def unique_id: String = {
        var unique_id: String = c.id

        if (ground.nonEmpty) {
            unique_id += "("
            var first = true
            for (v <- ground.keys)
                if (first) {
                    unique_id += v + "=" + ground(v)
                    first = false
                } else {
                    unique_id += "," + v + "=" + ground(v)
                }

            unique_id += ")"
        }

        unique_id
    }
}


sealed abstract class EvoOperator
case class Deprec_AddEvoOperator(add : GroundPredicate) extends EvoOperator
case class Deprec_RemoveEvoOperator(rmv : GroundPredicate) extends EvoOperator
case class Deprec_RemoveAllEvoOperator(rmv_all : String) extends EvoOperator
case class AddOperator(p : Predicate) extends EvoOperator
case class RmvOperator(p : Predicate) extends EvoOperator








