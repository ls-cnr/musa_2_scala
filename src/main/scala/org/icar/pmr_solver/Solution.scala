package org.icar.pmr_solver

import org.icar.musa.context.StateOfWorld



class SolutionSet(val initial_node : Node, var goal_supervisor : GoalSupervisor) {

  /* errore l'init deve avvenire con un unico WTS che contiente lo stato iniziale */
  var wts_list : List[ExpWTS] = List(init(initial_node))


  def init(node : Node) : ExpWTS = new ExpWTS(node,goal_supervisor)

  def update(focus : Node, exp_due_to_system: List[SystemExpansion], exp_due_to_environment: List[EnvironmentExpansion]): Unit = {
    var new_wts_list = List.empty

    for (wts : ExpWTS <- wts_list)
      new_wts_list = wts.update(focus, exp_due_to_system,exp_due_to_environment) :: new_wts_list

    wts_list = new_wts_list
  }


}


case class Node(state : StateDescription)
case class Transition(origin : Node, destination : Node, capability: SystemAction, scenario_name : String)
case class PerturbationArc(origin : Node, destination : Node, probability : Float, env_perturbation: EnvironmentAction, scenario_name : String)

/* Luca: this class could be improve is immutable, and update is a recursive function */
class ExpWTS(val initial_node : Node, var goal_supervisor : GoalSupervisor) {

  var nodes : Set[Node] = Set(initial_node)
  var transitions : Set[Transition] = Set.empty
  var perturbations : Set[PerturbationArc] = Set.empty

  var degree_of_sat : Double = 0

  def update(focus_node : Node, exp_due_to_system: List[SystemExpansion], exp_due_to_environment: List[EnvironmentExpansion]) : List[ExpWTS] = {
    var updated_list : List[ExpWTS] = List.empty

    /* IF this WTS is involved into the system expansion */
    if (nodes.contains(focus_node)) {

      /* THEN split the WTS for applying all the expansions due to system actions */
      for (exp <- exp_due_to_system) {
        val clone_wts = this.clone().asInstanceOf[ExpWTS]   /* test THIS */

        for (evolution_part <- exp.trajectory) {
          if (!clone_wts.nodes.contains(evolution_part.dest))
            clone_wts.nodes = clone_wts.nodes + evolution_part.dest

          transitions = transitions + Transition(exp.from, evolution_part.dest, exp.due_to, evolution_part.name)
        }

        /* AND check for applying all the expansions due to environment actions */
        for (pert <- exp_due_to_environment) {
          for (perturb_part <- pert.probtrajectory) {
            if (!clone_wts.nodes.contains(perturb_part.dest))
              clone_wts.nodes = clone_wts.nodes + perturb_part.dest

            perturbations = perturbations + PerturbationArc(pert.from, perturb_part.dest, perturb_part.probability, pert.due_to, perturb_part.name)
          }
        }

        /* FINALLY, the new list of WTS will contain the cloned updated WTS */
        updated_list = clone_wts :: updated_list
      }

    /* OTERWISE leave the original WTS into the new list of WTS */
    } else {

      updated_list = this :: updated_list

    }

    /* RETURN the list of updated WTS (spitted and originals) */
    updated_list
  }

}




class Expansion
case class SystemExpansion(due_to : SystemAction, from : Node, trajectory : Array[Evo])
case class EnvironmentExpansion(due_to : EnvironmentAction, from : Node, probtrajectory : Array[ProbabilisticEvo])

case class Evo (name: String, dest : Node)
case class ProbabilisticEvo (name: String, probability : Float, dest : Node)

