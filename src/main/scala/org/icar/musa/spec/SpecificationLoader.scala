package org.icar.musa.spec

import org.icar.fol.AssumptionSet
import org.icar.musa.context.StateOfWorld
import org.icar.musa.pmr.QualityAsset

abstract class DomainProperty

abstract class SessionProperty extends DomainProperty
case class SingleSession() extends SessionProperty
case class MultiSession() extends SessionProperty

abstract class GrounderProperty extends DomainProperty
case class EndToEnd() extends GrounderProperty
case class OnDemand() extends GrounderProperty

abstract class WTSExplorationProperty extends DomainProperty
case class EarlyWTSExploration() extends WTSExplorationProperty
case class LateWTSExploration() extends WTSExplorationProperty

abstract class SolutionProperty extends DomainProperty
case class AllInOneWorkflow() extends SolutionProperty
case class ManyAlternativeWorkflows() extends SolutionProperty



abstract class SpecificationLoader {
  def domains : Array[DomainLoader]
}

abstract class DomainLoader {
  def name : String
  def initial_state : StateOfWorld

  def assumption : AssumptionSet
  def goal : LTLGoal
  def quality_asset : QualityAsset

  def abstract_repository : Array[AbstractCapability]
  def concrete_repository : Array[ConcreteCapabilityFactory]

  def selection_strategy : Option[SelectionStrategy] = None

  def session_type : SessionProperty = SingleSession()
  def grounder_type : GrounderProperty = EndToEnd()
  def wts_exploration_type : WTSExplorationProperty = LateWTSExploration()
  def solution_type : SolutionProperty = ManyAlternativeWorkflows()

  def active : Boolean = true


  def recover_abstract(str: String, repository: Array[AbstractCapability]): Option[GroundedAbstractCapability] = {
    var cap : Option[GroundedAbstractCapability] = None

    for (c <- repository if c.name==str)
      cap = Some(c.asInstanceOf[GroundedAbstractCapability])

    cap
  }

}

