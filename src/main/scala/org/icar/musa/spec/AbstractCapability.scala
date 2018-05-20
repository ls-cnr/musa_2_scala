package org.icar.musa.spec

import net.sf.tweety.logics.fol.semantics.HerbrandInterpretation
import org.icar.fol.{ConstantTerm, FOLCondition, VariableTerm}
import org.icar.musa.context.EvoOperator

import scala.collection.mutable.ArrayBuffer

abstract class AbstractCapability(val name : String, val pre: FOLCondition, val post: FOLCondition)

case class GroundedAbstractCapability(override val name:String, override val pre: FOLCondition, override val post: FOLCondition, scenarios : Map[String,EvolutionScenario], grounding:Map[VariableTerm,ConstantTerm]=Map() ) extends AbstractCapability(name,pre,post)
case class ParametricAbstractCapability (override val name:String, params: ArrayBuffer[VariableTerm], override val pre: FOLCondition, override val post: FOLCondition, scenarios : Map[String,EvolutionScenario] ) extends AbstractCapability(name,pre,post) {

  def ground(interpretation : HerbrandInterpretation) : GroundedAbstractCapability = ???

}


case class EvolutionScenario(evo : Array[EvoOperator])

