package org.icar.pmr_solver

import org.icar.fol.Assumption
import org.icar.musa.main_entity.GroundedAbstractCapability
import org.icar.musa.scenarios.SPSScenario
import org.icar.pmr_solver.Test_Solver_IDSlike.{env_action, goalmodel, initial, my_domain, my_problem, solver, sys_action}

object test_Solver_SPS extends App {

	val domain = new SPSScenario("./data/sps_data")

	val axioms: Array[Assumption] = domain.axioms_set
	def qos(n:Node):Float=0
	val my_domain = Domain(Array.empty,Array.empty,axioms,qos)

	var action_list : List[SystemAction] = List.empty
	for (c<-domain.capabilities) {
		action_list = MUSA_entities.capability_to_system_actions(c.asInstanceOf[GroundedAbstractCapability]) ::: action_list
	}
	val sys_action = action_list.toArray

	val env_action : Array[EnvironmentAction] = Array()


	val initial= domain.initial_state
	val goalmodel = LTLGoalSet(Array(MUSA_entities.ltlFormula_to_LTLformula(domain.goal_specification.ltl)))
	val available = AvailableActions(sys_action,env_action)

	val my_problem = Problem(initial,goalmodel,available)

	/* the solver */
	val solver = new Solver(my_problem,my_domain)

	val its=solver.iterate_until_termination(IterationTermination(3))

	println("Number of iterations: "+its)
	println("Number of generated WTS: "+solver.solution_set.wts_list.size)
	println("Number of full WTS: "+solver.solution_set.full_wts.size)
	println("Number of partial WTS: "+solver.solution_set.partial_wts.size)

	println( solver.solution_set.all_solutions_to_graphviz(node => node.w.toString) )

}

