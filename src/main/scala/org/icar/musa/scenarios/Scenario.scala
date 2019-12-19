package org.icar.musa.scenarios

import org.icar.fol.AssumptionSet
import org.icar.musa.context.StateOfWorld
import org.icar.musa.main_entity.{AbstractCapability, LTLGoal}
import org.icar.musa.pmr.{QualityAsset, SingleGoalProblemSpecification, TerminationDescription}

trait Scenario {
  def problem: SingleGoalProblemSpecification = {
    val ass_set = assumption_set
    val goal = goal_specification
    val asset = quality_asset
    SingleGoalProblemSpecification(ass_set,goal_specification,asset)
  }

  def assumption_set : AssumptionSet
  def goal_specification : LTLGoal
  def quality_asset : QualityAsset

  def initial_state: StateOfWorld
  def capabilities : Array[AbstractCapability]
  def termination: TerminationDescription
}
