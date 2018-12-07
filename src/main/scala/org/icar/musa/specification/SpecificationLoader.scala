package org.icar.musa.specification

import org.icar.fol.{AssumptionLoader, AssumptionSet}
import org.icar.musa.context.StateOfWorld
import org.icar.musa.pmr.QualityAsset
import org.icar.musa.main_entity._

import scala.io.Source



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

  def selection_strategy : Option[SelectionStrategy] = None     // strategy for many-alternative-workflows
  def validation_strategy : Option[ValidationStrategy] = None   // strategy for many-alternative-workflows
  def proxy_strategy : Option[ProxyStrategy] = None             // strategy for multi-session

  def session_type : SessionProperty = SingleSession()
  def grounder_type : GrounderProperty = EndToEnd()
  def wts_exploration_type : WTSExplorationProperty = LateWTSExploration()
  def solution_type : SolutionProperty = ManyAlternativeWorkflows()

  def active : Boolean = true



  protected def recover_abstract(str: String, repository: Array[AbstractCapability]): Option[GroundedAbstractCapability] = {
    var cap : Option[GroundedAbstractCapability] = None

    for (c <- repository if c.name==str)
      cap = Some(c.asInstanceOf[GroundedAbstractCapability])

    cap
  }

}



abstract class DefaultDomainLoader(ass_file: String,goal_file: String,cap_file: String) extends DomainLoader {

  override def assumption: AssumptionSet = load_ass_from_file(ass_file)
  override def goal: LTLGoal = load_goal_from_file(goal_file)
  override def abstract_repository: Array[AbstractCapability] = load_cap_from_file(cap_file)

  private def load_cap_from_file(file: String): Array[AbstractCapability] = {
    val s = Source.fromFile(file)
    val parser = new AbstractCapabilityParser()
    val p = parser.parseAll(parser.cap_specification,s.mkString)

    p.get.toArray
  }
  private def load_goal_from_file(file: String): LTLGoal = {
    val s = Source.fromFile(file)
    val parser = new LTLGoalParser()
    val p: parser.ParseResult[LTLGoal] = parser.parseAll(parser.goal,s.mkString)
    p.get
  }
  private def load_ass_from_file(file: String): AssumptionSet = AssumptionLoader.load_from_file(file)

}




