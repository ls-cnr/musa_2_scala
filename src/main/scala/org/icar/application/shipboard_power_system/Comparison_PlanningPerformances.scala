package scala.org.icar.application.shipboard_power_system

import java.io.{File, PrintWriter}

import org.icar.application.shipboard_power_system.{ForceField, SPSCircuit}
import org.icar.pmr_solver.high_level_specification.{AvailableActions, Domain, LTLGoalSet, Problem}
import org.icar.pmr_solver.nmc.{NMCSolver, WTSTreeNode}
import org.icar.pmr_solver.symbolic_level.{HL2Raw_Map, RawState}

import scala.org.icar.pmr_solver.best_first_planner.{SolutionConfiguration, Solver, SolverConfiguration, TimeTermination}
import scala.org.icar.symbolic_level.R2S

object Comparison_PlanningPerformances extends App {
	if (args.size==0) {
		println("Usage:")
		println("-in input_file")
		println("-out output_file")
		println("-ng number_of_random_goals")
		println("-mit number_of_montecarlo_repetitions")
		println("")
		//println("Using Defaults")

		//comparison("./data/sps_data/circuit3.txt","./data/sps_data/stats.csv",3,20)
	} else if (args.size == 8) {
		val input_file = args(1)
		val output_file = args(3)
		val num_goals = args(5).toInt
		val num_its = args(7).toInt

//		println("-in ",input_file)
//		println("-out ", output_file)
//		println("-ng ",num_goals)
//		println("-mit ",num_its)

		comparison(input_file,output_file,num_its,num_goals)
	}

	//val MAX_ITERATIONS = 20
	//val file = "/Users/luca/Dropbox/stats.csv"


	def comparison(circuit_file:String, out_file:String, MAX_ITERATIONS:Int, MAX_GOALS : Int): Unit = {
		// Medium Circuit
		val circuit = SPSCircuit.build_from_file(circuit_file) //prepare_circuit
		val initial = SPSCircuit.circuit_3_initial_totally_switched_off//circuit_3_initial_simple_failure

		val domain = Domain("SPS",circuit.generate_predicates,circuit.generate_domain_types,circuit.generate_axioms)
		val map = new HL2Raw_Map(domain)

		val available = AvailableActions(circuit.generate_actions,Array.empty)

		val time_to_complete_array = List(500,1000,2000,5000,10000)


		val pw = new PrintWriter(new File(out_file))
		pw.println("method, time, visited, solutions")

		for (i <- 1 to MAX_GOALS) {
			println("it "+i)
			val mission = SPSCircuit.random_mission
			val force_field = new ForceField(circuit,mission,map)
			val goal = circuit.generate_goal(mission)
			println("Goal="+goal)
			val goalmodel = LTLGoalSet(Array(goal))
			val specifications = map.ltl_formula(goal)
			val problem = Problem(initial,goalmodel,available)

			for (t<-time_to_complete_array) {
				generate_statistics_for_planner("domain_heuristics",problem,domain,t,force_field.qos)
				generate_statistics_for_planner("best_first_R2S",problem,domain,t,R2S.metric(specifications))
				generate_statistics_for_MCST(problem,domain,t)
				generate_statistics_for_MCST_R2S(problem,domain,t)
			}
		}
		pw.close



		def generate_statistics_for_planner(method:String,problem: Problem, domain: Domain, t: Int, qos : RawState => Float):Unit = {
			val solver = Solver(problem,domain,qos)

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
				print("x")
			}
			val visited : Float = total_visited/MAX_ITERATIONS
			val solutions : Float = total_solutions/MAX_ITERATIONS

			pw.println(s"montecarlo, $t, $visited, $solutions")
			println("x")

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
				print("y")
			}
			val visited : Float = total_visited/MAX_ITERATIONS
			val solutions : Float = total_solutions/MAX_ITERATIONS
			pw.println(s"montecarloR2S, $t, $visited, $solutions")
			println("y")
		}

	}

}
