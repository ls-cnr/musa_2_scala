package org.icar.musa.spec

import org.icar.fol.AssumptionSet
import org.icar.musa.context.StateOfWorld
import org.icar.musa.pmr.QualityAsset

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
  def concrete_repository : Array[ConcreteCapability]

  def recover_abstract(str: String, repository: Array[AbstractCapability]): Option[GroundedAbstractCapability] = {
    var cap : Option[GroundedAbstractCapability] = None

    for (c <- repository if c.name==str)
      cap = Some(c.asInstanceOf[GroundedAbstractCapability])

    cap
  }
}

