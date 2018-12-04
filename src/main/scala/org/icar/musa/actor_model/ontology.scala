package org.icar.musa.actor_model

import akka.actor.ActorRef
import org.icar.musa.context._
import org.icar.musa.pmr.Solution
import org.icar.musa.spec.EvolutionScenario

case class DomainInfo(id : Int)


case class ContextUpdate(env : EnvContext )
case class SimulatedStateUpdate(scn: EvolutionScenario)
case class LocalStateUpdate(env : EnvContext)
case class MeasurablesUpdate(m : Measurables)


case class SelfConfigureRequest(wi : StateOfWorld)
case class TerminateSelfConfiguration()


case class SingleSolution( s : Solution )
case class MultiSolution( ms : Set[Solution] )


case class TaskCompleted(abstract_capability_name : String, scenario_name : String )


case class SearchConcrete(abs_name: String)
case class SearchAllComplete( s: Solution )
case class MappingConcrete(abs_name: String, provider: ActorRef)
case class MappingAllConcrete(worker_grounding: Map[String, ActorRef])
case class UncoveredConcrete(abs_name: String)
case class UncoveredConcretes()
case class ReplaceConcrete(abs_name: String)



//case class RegisterProvider(ref : ActorRef, abs_name: String)
//case class UnregisterProvider(ref : ActorRef, abs_name: String)
case class CallForProviders(abs_name: String, requestor : ActorRef)
case class ProviderResponse(abs_name : String, provider : ActorRef)
case class SelectedForAbstractCapability(abs_name: String, requestor:ActorRef)
case class WorkerInstance(abs_name : String, worker : ActorRef)


case class Validate(solution: Solution)
case class ValidatedAndSelected(solution: Solution)


case class RegisterNewVariable(name: String, init_value: Any)

case class UpdateVariableValue(name: String, value: Any)

case class QueryVariableValue(name: String)


case class RequestNewSession(in : DataIn, wi : StateOfWorld)