package org.icar.musa.actor

import org.icar.musa.context.StateOfWorld
import org.icar.musa.pmr.Solution
import org.icar.musa.spec.EvolutionScenario

case class DBInfo(url:String, driver:String, user:String, psw:String)

case class StateUpdate( w : StateOfWorld )
case class SimulatedStateUpdate(scn: EvolutionScenario)


case class SelfConfigureRequest(wi : StateOfWorld)


case class SingleSolution( s : Solution )
case class MultiSolution( ms : Set[Solution] )




case class Completed( abstract_capability_name : String, scenario_name : String )
