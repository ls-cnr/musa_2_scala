package org.icar.pmr_solver

import net.sf.tweety.logics.fol.semantics.HerbrandInterpretation
import net.sf.tweety.logics.fol.syntax.FOLAtom
import net.sf.tweety.lp.asp.syntax.Program
import org.icar.fol.Entail.{head_for_asl, solver, tx}
import org.icar.musa.context.StateOfWorld


/******* SOLUTIONS ********/

class SolutionSet(val initial_w : StateOfWorld, domain : Domain, val goals : LTLGoalSet) {

  private val base = new Program(domain.axioms_as_rulelist)

  var wts_list : List[ExpWTS] = init()

  private def init() : List[ExpWTS] = {
    val initial_state = state_checkin(initial_w)
    val supervisors = goals.getSupervisors(initial_state)
    val mapping = StateLabel(initial_state,supervisors,0)
    List(ExpWTS(Set(initial_state),Set.empty,Set.empty,List(mapping),0))
  }


  /* The node definition is common for all the WTS, thus it is created --once for all-- when a new state of world is generated */
  def state_checkin(w : StateOfWorld) : Node = {
    val w_base = base.clone()
    for (s <- w.statements)
      w_base.addFact(head_for_asl(s))

    val response = solver.computeModels(w_base, 10000)
    val interpretation = new HerbrandInterpretation()
    if (response != null) {
      val as = response.get(0)
      val it = as.iterator()

      while (it.hasNext) {
        val f = tx.toFOL(it.next())
        interpretation.add(f.asInstanceOf[FOLAtom])
      }


    }

    Node(w,interpretation)
  }


  /*
   * returns the most promising node to be expanded.

   * for all the WTS(s),
   *  explore the wts frontier and
   *  get the node with highest metric
   */
  def get_next_node : Node = {
    var node : Node = null

    var node_value : Float = -1

    for (wts <- wts_list)
      for (f <- wts.frontier)
        if (f.metric > node_value) {
          node = f.state
          node_value = f.metric
        }

    node
  }


  /* given a focus node and a set of expansions, it updates all the corresponsing WTS where the exp(s) apply */
  def update_the_wts_list(focus : Node, exp_due_to_system: List[SystemExpansion], exp_due_to_environment: List[EnvironmentExpansion]): Unit = {
    var new_wts_list : List[ExpWTS] = List.empty

    for (wts : ExpWTS <- wts_list)
      new_wts_list = split_wts(wts,focus, exp_due_to_system,exp_due_to_environment) ::: new_wts_list

    wts_list = new_wts_list
  }

  /*
   * This function is the core of WTS expansion:
   * expanding a single WTS often means split it into a number of alternative possibilities
   */
  def split_wts(wts : ExpWTS, focus_node : Node, exp_due_to_system: List[SystemExpansion], exp_due_to_environment: List[EnvironmentExpansion]) : List[ExpWTS] = {
    var updated_list : List[ExpWTS] = List.empty

    /* IF this WTS is involved into the system expansion */
    if (wts.nodes.contains(focus_node)) {

      /* THEN split the WTS for applying all the expansions due to system actions */
      for (exp <- exp_due_to_system) {
        var new_nodes : Set[Node] = Set.empty
        var new_transition : Set[TransitionArc] = Set.empty
        var new_perturb : Set[PerturbationArc] = Set.empty

        for (evolution_part <- exp.trajectory) {

          if (!wts.nodes.contains(evolution_part.dest)) {
            //mapping(evolution_part.dest)
            new_nodes = new_nodes + evolution_part.dest
          }
          new_transition = new_transition + TransitionArc(exp.from, evolution_part.dest, exp.due_to, evolution_part.name)
        }

        /* AND check for applying all the expansions due to environment actions */
        for (pert <- exp_due_to_environment) {
          for (perturb_part <- pert.probtrajectory) {
            if (!wts.nodes.contains(perturb_part.dest))
              new_nodes = new_nodes + perturb_part.dest

            new_perturb = new_perturb + PerturbationArc(pert.from, perturb_part.dest, perturb_part.probability, pert.due_to, perturb_part.name)
          }
        }

        val updated_frontier : List[StateLabel] = update_mapping(wts,focus_node,new_nodes,new_transition,new_perturb)
        val quality = calculate_quality_of_solution(wts,focus_node,updated_frontier,new_nodes,new_transition,new_perturb)

        /* FINALLY, the new list of WTS will contain the cloned updated WTS */
        updated_list = ExpWTS(wts.nodes++new_nodes,wts.transitions++new_transition,wts.perturbations++new_perturb,updated_frontier,quality) :: updated_list
      }

      /* OTERWISE leave the original WTS into the new list of WTS */
    } else {

      updated_list = wts :: updated_list

    }

    /* RETURN the list of updated WTS (spitted and originals) */
    updated_list
  }



  /*
   * This function calculates the frontier of the new WTS
   *
   * The new frontier is computed by removing the focus_node and
   * adding all the new generated node, updating goals and qos
  */
  def update_mapping(old_wts: ExpWTS, focus_node: Node, new_nodes: Set[Node], new_transition: Set[TransitionArc], new_perturb: Set[PerturbationArc]): List[StateLabel] = {
    var focus_label : StateLabel = old_wts.frontier.find( _.state == focus_node).get

    /* remove focus node from frontier */
    var new_frontier : List[StateLabel] = old_wts.frontier.filter(_ != focus_label)

    /* add new nodes to the frontier */
    for (node <- new_nodes) {
      val updated_array: Array[GoalSupervisor] = for (l <- focus_label.sup_array) yield l.getNextSupervisor(node)
      val updated_metric : Float = domain.qos(node)
      new_frontier = StateLabel(node,updated_array,updated_metric) :: new_frontier
    }

    new_frontier
  }


  /*
   * This function calculates the quality of the new WTS
   *
   * PROVVISORIAMENTE: quality = average of frontier node values
   */
  def calculate_quality_of_solution(old_wts: ExpWTS, focus_node: Node, updated_frontier : List[StateLabel], new_nodes: Set[Node], new_transition: Set[TransitionArc], new_perturb: Set[PerturbationArc]): Float = {
    var q : Float = 0
    for (f <- updated_frontier)
      q+=f.metric

    q/updated_frontier.size
  }


}


/******* WTS ********/

case class ExpWTS(
                   nodes : Set[Node],
                   transitions : Set[TransitionArc],
                   perturbations : Set[PerturbationArc],
                   frontier : List[StateLabel],
                   quality_of_solution : Float
                 )
case class Node(w : StateOfWorld, interpretation : HerbrandInterpretation)
case class TransitionArc(origin : Node, destination : Node, action: SystemAction, scenario_name : String)
case class PerturbationArc(origin : Node, destination : Node, probability : Float, env_action: EnvironmentAction, scenario_name : String)

case class StateLabel(state : Node, sup_array : Array[GoalSupervisor], metric : Float)


/******* STATE EVOLUTIONS ********/

class Expansion
case class SystemExpansion(due_to : SystemAction, from : Node, trajectory : Array[Evo])
case class EnvironmentExpansion(due_to : EnvironmentAction, from : Node, probtrajectory : Array[ProbabilisticEvo])
case class Evo (name: String, dest : Node)
case class ProbabilisticEvo (name: String, probability : Float, dest : Node)

