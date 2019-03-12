package org.icar.musa.scenarios.sps

import org.icar.fol.AssumptionSet
import org.icar.musa.context.StateOfWorld
import org.icar.musa.main_entity._
import org.icar.musa.pmr.QualityAsset
import org.icar.musa.scenarios.SPSScenario
import org.icar.musa.specification._


/* path = "./sps_data" */
class SPSSpecLoader(path: String) extends SpecificationLoader {
  override def domains: Array[DomainLoader] = {
    Array(new SPSScenario1(path))
  }
}


class SPSScenario1(generalpath: String) extends DomainLoader {
  val path: String = generalpath+"/sps_data"
  val domain: SPSScenario = new SPSScenario(path)

  override def session_type : SessionProperty = SingleSession()
  override def grounder_type : GrounderProperty = EndToEnd()
  override def wts_exploration_type : WTSExplorationProperty = LateWTSExploration()
  override def solution_type : SolutionProperty = LateDecisionWorkflows()


  override def name: String = "circuit3_mission1_nofailures"
  override def initial_state: StateOfWorld = domain.initial_state
  override def assumption: AssumptionSet = domain.assumption_set
  override def goal: LTLGoal = domain.goal_specification
  override def quality_asset: QualityAsset = domain.quality_asset
  override def abstract_repository: Array[AbstractCapability] = domain.capabilities
  override def concrete_repository: Array[ConcreteCapabilityFactory] = {
    val conc_rep = new SPSConcreteRepository(domain.circuit,domain.scenario,abstract_repository)
    conc_rep.repository
  }

  override def selection_strategy : Option[SelectionStrategy] = Some(new SPSSelectionStrategy(quality_asset.pretty_string, quality_asset.evaluate_state) )

  override def validation_strategy: Option[ValidationStrategy] = Some(new SPSValidationStrategy(domain)) // None

  override def active: Boolean = true

}




