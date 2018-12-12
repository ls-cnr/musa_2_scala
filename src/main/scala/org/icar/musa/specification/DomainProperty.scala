package org.icar.musa.specification

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
case class EarlyDecisionWorkflow() extends SolutionProperty
case class LateDecisionWorkflows() extends SolutionProperty
