package org.icar.pmr_solver

import net.sf.tweety.logics.fol.semantics.HerbrandInterpretation
import net.sf.tweety.logics.fol.syntax.FOLAtom
import net.sf.tweety.lp.asp.syntax.Program
import org.icar.fol.Entail.{head_for_asl, solver, tx}
import org.icar.musa.context.StateOfWorld
import org.icar.musa.pmr.TerminationDescription

class Solver(
      val problem: Problem,
      val domain: Domain,
      val actions : AvailableActions,
      val termination : TerminationDescription
            ) {

  var initial_node = state_checkin(problem.I)
  val base = new Program(domain.axioms_as_rulelist)

  var solution_set = new SolutionSet(initial_node, problem.goal_model.getSupervisor)
  var frontier : List[Node] = List.empty        /* move this into the ExpWTS class */
  var mappings : List[StateDescription] = List.empty


  private def state_checkin(w : StateOfWorld) : Node = {
    val w_base = base.clone()
    for (s <- w.statements)
      base.addFact(head_for_asl(s))

    val response = solver.computeModels(base, 10000)
    val interpretation = new HerbrandInterpretation()
    if (response != null) {
      val as = response.get(0)
      val it = as.iterator()

      while (it.hasNext) {
        val f = tx.toFOL(it.next())
        interpretation.add(f.asInstanceOf[FOLAtom])
      }
    }

    val descr = StateDescription(w,interpretation)
    mappings = descr :: mappings
    Node(descr)
  }

  /* let us try this change:
      1) get most promising WTS,
      2) get most risky node or (in absence) the most promising node
        [AND TRY ALSO VICEVERSA]
  */
  private def get_next_node : Node = {
    require(frontier.nonEmpty)

    val node = frontier.head
    frontier = frontier.tail

    node
  }

  private def applicable_capabilities(node : Node) : List[SystemAction] = {
    var list : List[SystemAction] = List.empty

    for (action <- actions.sys_action) {
      val apply = node.state.interpretation.satisfies(action.pre)
      if (apply)
        list = action :: list
    }

    list
  }

  private def applicable_perturbations(node : Node) : List[EnvironmentAction] = {
    var list : List[EnvironmentAction] = List.empty

    for (action <- actions.env_action) {
      val apply = node.state.interpretation.satisfies(action.pre)
      if (apply)
        list = action :: list
    }

    list
  }

  private def generate_system_expansion( node : Node, action : SystemAction) : SystemExpansion = {
    def calculate_evolution(node : Node, evo_description : EvolutionGrounding) : Evo = {
      val w2 = StateOfWorld.extend(node.state.w, evo_description.evo)
      val n2 = state_checkin(w2)
      Evo(evo_description.name,n2)
    }

    val trajectory: Array[Evo] = for (effect <- action.effects) yield calculate_evolution(node,effect)
    SystemExpansion(action,node,trajectory)
  }

  private def generate_environment_expansion( node : Node, action : EnvironmentAction) : EnvironmentExpansion = {

    def calculate_probabiliostic_evolution(node : Node, evo_description : ProbabilisticEvolutionGrounding) : ProbabilisticEvo = {
      val w2 = StateOfWorld.extend(node.state.w, evo_description.evo)
      val n2 = state_checkin(w2)
      ProbabilisticEvo(evo_description.name,evo_description.probability,n2)
    }

    val trajectory: Array[ProbabilisticEvo] = for (effect <- action.effects) yield calculate_probabiliostic_evolution(node,effect)
    EnvironmentExpansion(action,node,trajectory)
  }





  def iteration : Unit = {
    val node = get_next_node

    val actions = applicable_capabilities(node)
    val envs = applicable_perturbations(node)

    var exp_due_to_system : List[SystemExpansion] = List.empty
    for (a <- actions) {
      exp_due_to_system = generate_system_expansion(node,a) :: exp_due_to_system
    }

    var exp_due_to_environment : List[EnvironmentExpansion] = List.empty
    for (e <- envs) {
      exp_due_to_environment = generate_environment_expansion(node,e) :: exp_due_to_environment
    }

    solution_set.update(node,exp_due_to_system,exp_due_to_environment)

  }

}




case class StateDescription(w : StateOfWorld, interpretation : HerbrandInterpretation)
