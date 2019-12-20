package org.icar.high_level_specification

import org.icar.pmr_solver.high_level_specification.{DomainArgument, GroundPredicate, HL_PredicateFormula, Predicate, True}

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

sealed abstract class EvoOperator
case class Deprec_AddEvoOperator(add : GroundPredicate) extends EvoOperator
case class Deprec_RemoveEvoOperator(rmv : GroundPredicate) extends EvoOperator
case class Deprec_RemoveAllEvoOperator(rmv_all : String) extends EvoOperator
case class AddOperator(p : Predicate) extends EvoOperator
case class RmvOperator(p : Predicate) extends EvoOperator
