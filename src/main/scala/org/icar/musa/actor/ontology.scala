package org.icar.musa.actor

import akka.actor.ActorRef
import org.icar.musa.context.StateOfWorld
import org.icar.musa.pmr.Solution
import org.icar.musa.spec.EvolutionScenario

case class DBInfo(url:String, driver:String, user:String, psw:String)
case class DomainInfo(id : Int)

case class StateUpdate( w : StateOfWorld )
case class SimulatedStateUpdate(scn: EvolutionScenario)


case class SelfConfigureRequest(wi : StateOfWorld)


case class SingleSolution( s : Solution )
case class MultiSolution( ms : Set[Solution] )


case class TaskCompleted(abstract_capability_name : String, scenario_name : String )


case class SearchConcrete(abs_name: String)
case class ReplaceConcrete(abs_name: String)
case class MappingConcrete(abs_name: String, provider: ActorRef)
case class UncoveredConcrete(abs_name: String)


//case class RegisterProvider(ref : ActorRef, abs_name: String)
//case class UnregisterProvider(ref : ActorRef, abs_name: String)
case class CanProviderDoAbstractCapability(abs_name: String, requestor : ActorRef)
case class ProviderResponse(abs_name : String, provider : ActorRef)
case class ProviderHasBeenSelectedForAbstractCapability(abs_name: String, requestor:ActorRef)
case class WorkerInstanceForEmployer(abs_name : String, worker : ActorRef)


case class Validate(solution: Solution)