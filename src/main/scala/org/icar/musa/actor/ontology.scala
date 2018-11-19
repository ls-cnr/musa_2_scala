package org.icar.musa.actor

import org.icar.musa.context.StateOfWorld
import org.icar.musa.pmr.Solution
import org.icar.musa.spec.{ConcreteCapability, EvolutionScenario, GroundedAbstractCapability}

case class DBInfo(url:String, driver:String, user:String, psw:String)
case class DomainInfo(id : Int)

case class StateUpdate( w : StateOfWorld )
case class SimulatedStateUpdate(scn: EvolutionScenario)


case class SelfConfigureRequest(wi : StateOfWorld)


case class SingleSolution( s : Solution )
case class MultiSolution( ms : Set[Solution] )


case class TaskCompleted(abstract_capability_name : String, scenario_name : String )


case class AskConcrete(abs_name: String)
case class AskForceOtherConcrete(abs_name: String)
case class MappingAbstractConcrete(abs_name: String, capability: ConcreteCapability)
