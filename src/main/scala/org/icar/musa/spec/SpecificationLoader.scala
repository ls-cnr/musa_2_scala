package org.icar.musa.spec

import org.icar.fol.AssumptionSet
import org.icar.musa.context.StateOfWorld
import org.icar.musa.pmr.QualityAsset
import org.icar.musa.scenarios.PRINWakeUpScenario
import org.icar.musa.scenarios.concrete.WakeUpConcreteRepository

abstract class SpecificationLoader {
  def domains : Array[DomainLoader]
}

class UPA4SAR_spec_loader extends SpecificationLoader {
  override def domains: Array[DomainLoader] = {
    Array(new UPA4SAR_domain_loader)
  }
}

abstract class DomainLoader {
  def name : String
  def initial_state : StateOfWorld

  def assumption : AssumptionSet
  def goal : LTLGoal
  def quality_asset : QualityAsset

  def abstract_repository : Array[AbstractCapability]
  def concrete_repository : Array[ConcreteCapability]
}

class UPA4SAR_domain_loader extends DomainLoader {
  val sc = new PRINWakeUpScenario
  val mk = new WakeUpConcreteRepository

  override def name: String = "WakeUp"

  override def initial_state: StateOfWorld = sc.initial_state

  override def assumption: AssumptionSet = sc.assumption_set

  override def goal: LTLGoal = sc.goal_specification

  override def quality_asset: QualityAsset = sc.quality_asset

  override def abstract_repository: Array[AbstractCapability] = sc.capabilities

  override def concrete_repository: Array[ConcreteCapability] = mk.load_concrete_capabilty

}

