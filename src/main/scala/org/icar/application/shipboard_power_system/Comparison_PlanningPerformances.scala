package scala.org.icar.application.shipboard_power_system

import java.io.{File, PrintWriter}

import org.icar.application.shipboard_power_system.{ForceField, SPSCircuit}
import org.icar.pmr_solver.high_level_specification.{AvailableActions, Domain, LTLGoalSet, Problem}
import org.icar.pmr_solver.nmc.{NMCSolver, R2S, WTSTreeNode}
import org.icar.pmr_solver.symbolic_level.{HL2Raw_Map, RawState}
import org.icar.pmr_solver.{SolutionConfiguration, SolverConfiguration, TimeTermination}

import scala.org.icar.pmr_solver.best_first_planner.{SolutionConfiguration, Solver, SolverConfiguration, TimeTermination}

object Comparison_PlanningPerformances extends App {
	val MAX_ITERATIONS = 20
	val file = "/Users/luca/Google Drive/attività ricerca/montecarlosearch/stats.csv"
	val pw = new PrintWriter(new File(file))
	//val pw = new PrintWriter(new File("./data/sps_data/stats.txt"))
	pw.println("method, time, visited, solutions")

	val termin_array = List(500,1000,2000,5000,10000)

	// Medium Circuit
	val circuit = SPSCircuit.build_from_file("./data/sps_data/circuit3.txt") //prepare_circuit
	val mission = SPSCircuit.circuit_3_mission
	val initial = SPSCircuit.circuit_3_initial_totally_switched_off//circuit_3_initial_simple_failure

	val domain = Domain(circuit.generate_predicates,circuit.generate_domain_types,circuit.generate_axioms)
	val map = new HL2Raw_Map(domain)
	val force_field = new ForceField(circuit,mission,map)

	val goal = circuit.generate_goal(mission)
	val goalmodel = LTLGoalSet(Array(goal))
	val specifications = map.ltl_formula(goal)

	val available = AvailableActions(circuit.generate_actions,Array.empty)
	val problem = Problem(initial,goalmodel,available)



	for (t<-termin_array) {
		generate_statistics_for_planner("plannerFF",problem,domain,t,force_field.qos)
		generate_statistics_for_planner("plannerR2S",problem,domain,t,R2S.metric(specifications))
		generate_statistics_for_MCST(problem,domain,t)
		generate_statistics_for_MCST_R2S(problem,domain,t)
	}

	pw.close


	def generate_statistics_for_planner(method:String,problem: Problem, domain: Domain, t: Int, qos : RawState => Float):Unit = {
		val solver = new Solver(problem,domain,qos)

		solver.iterate_until_termination(
			SolverConfiguration(
				TimeTermination(t),
				SolutionConfiguration(
					allow_self_loop = false,
					allow_cap_multiple_instance = true,
					allow_loop = false,
					allow_parallel_action = true
				)
			)
		)
		val visited = solver.num_nodes
		val solutions = solver.opt_solution_set.get.full_wts.size
		pw.println(s"$method, $t, $visited, $solutions")
	}

	def generate_statistics_for_MCST(problem: Problem, domain: Domain, t: Int):Unit = {
		var total_visited : Int = 0
		var total_solutions : Int = 0

		for (n<-1 to MAX_ITERATIONS) {
			val solver = new NMCSolver(problem,domain)
			solver.mcts(TimeTermination(t))
			total_visited += WTSTreeNode.id
			total_solutions += solver.solutions
			//println(solver.solutions)
		}
		val visited : Float = total_visited/MAX_ITERATIONS
		val solutions : Float = total_solutions/MAX_ITERATIONS

		pw.println(s"montecarlo, $t, $visited, $solutions")
	}

	def generate_statistics_for_MCST_R2S(problem: Problem, domain: Domain, t: Int):Unit = {
		var total_visited : Int = 0
		var total_solutions : Int = 0

		for (n<-1 to MAX_ITERATIONS) {
			val solver = new NMCSolver(problem,domain)
			solver.mcts_with_frontier(TimeTermination(t))
			total_visited += WTSTreeNode.id
			total_solutions += solver.solutions
			//println(solver.solutions)
		}
		val visited : Float = total_visited/MAX_ITERATIONS
		val solutions : Float = total_solutions/MAX_ITERATIONS
		pw.println(s"montecarloR2S, $t, $visited, $solutions")
	}
}
