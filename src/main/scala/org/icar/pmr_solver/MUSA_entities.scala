package org.icar.pmr_solver


/* Associate each action to the corresponsing Capability/Perturbation by giving a value to the params */
case class CapActionBinding(action: SystemAction, capability: Capability, grounding: ParamsGrounding)
case class PertActionBinding(action: EnvironmentAction, perturbation: Perturbation, grounding: ParamsGrounding)

case class ParamsGrounding(setting : Array[Grounding])
case class Grounding(variable : String, value : String)


/* operate the association */
object MUSA_entities {

  def capability_to_system_actions = ???

  def perturbation_to_environment_actions = ???

}

/* these should be defined outside */
class Capability {

}

class Perturbation {

}











