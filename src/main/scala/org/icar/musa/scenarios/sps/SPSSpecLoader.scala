package org.icar.musa.scenarios.sps

import javax.swing._
import org.icar.fol.AssumptionSet
import org.icar.musa.context.StateOfWorld
import org.icar.musa.pmr.{QualityAsset, Solution}
import org.icar.musa.scenarios.SPSScenario
import org.icar.musa.spec._

import scala.concurrent.duration._


/* path = "./sps_data" */
class SPSSpecLoader(path: String) extends SpecificationLoader {
  override def domains: Array[DomainLoader] = {
    Array(new SPSScenario1(path))
  }
}


class SPSScenario1(path: String) extends DomainLoader {
  val domain = new SPSScenario

  override def session_type : SessionProperty = SingleSession()
  override def grounder_type : GrounderProperty = EndToEnd()
  override def wts_exploration_type : WTSExplorationProperty = LateWTSExploration()
  override def solution_type : SolutionProperty = ManyAlternativeWorkflows()


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

  override def selection_strategy : Option[SelectionStrategy] = Some(new SPSSelectionStrategy)

  override def active: Boolean = true

}




