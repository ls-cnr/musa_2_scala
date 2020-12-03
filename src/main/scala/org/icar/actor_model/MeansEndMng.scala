package org.icar.actor_model

import akka.actor.{ActorRef, Props}
import org.icar.actor_model.core.{ApplicationConfig, MUSAActor, SolValidator}
import org.icar.actor_model.protocol.AbstractSolProtocol
import org.icar.actor_model.role.SolutionProducer
import org.icar.symbolic.StateOfWorld
import org.icar.pmr_solver.high_level_specification.{AvailableActions, Domain, LTLGoalSet}
import org.icar.pmr_solver.symbolic_level.{HL2Raw_Map, RawState}

import scala.org.icar.high_level_specification.Solution
import scala.org.icar.pmr_solver.best_first_planner.{Solver, SolverConfiguration, TimeTermination, WTSGraph}

class MeansEndMng(config:ApplicationConfig) extends MUSAActor
	with SolutionProducer {

	val my_log_area = config.logfactory.register_actor(self.path.name)
	mylog("welcome to the MeansEndMng !")

	val domain:Domain=config.domain
	val raw_convert = new HL2Raw_Map(domain)
	val available_actions:AvailableActions = config.availableAbstract

	var solution_map : Map[RawState,Array[Solution]] = Map.empty

	val validator_actor : Option[ActorRef] = init_validator_actor(config.validator)

	override def preStart(): Unit = {
		//mylog("welcome to MeansEndMng GUI !")
	}

	override def role__received_request_for_planning(sender: ActorRef, msg: AbstractSolProtocol.RequestSolutions): Unit = {
		val initial_state = msg.initial_state
		val goal_set = msg.goal_set

		val full_list_of_solutions = execute_planner(initial_state,goal_set)

		if (full_list_of_solutions.nonEmpty){
			if (validator_actor.isDefined)
				validator_actor.get ! delegate_validation(full_list_of_solutions.toArray)
			else
				context.parent ! reply_solutions(full_list_of_solutions.toArray)



		} else {
			context.parent ! reply_no_solutions
		}

	}


	private def execute_planner(initial_state: RawState, goal_set: LTLGoalSet) : List[Solution] = {
		val solver = Solver.mixed_factory(goal_set,available_actions,initial_state,domain,config.planner_heuristic)
		val solver_conf = SolverConfiguration(
			TimeTermination(config.planner_millisec),
			config.planner_config
		)
		solver.iterate_until_termination(solver_conf)
		val fullWTS = solver.opt_solution_set.get.full_wts.toList
		val state_of_world = raw_convert.inverse_state_of_world(initial_state.bit_descr)
		val fullSol = for (wts<-fullWTS) yield WTSGraph.WTStoSolution(wts,StateOfWorld(state_of_world))
		fullSol
	}

/*
	private def init_validators(validators:Array[SolValidator]): List[ActorRef] = {
		val array = for (v<-validators) yield init_validator_actor(v)
		array.toList
	}
*/
	private def init_validator_actor(opt_validator : Option[SolValidator]) : Option[ActorRef] = {
		if (opt_validator.isDefined) {
			val validator = opt_validator.get
			val props = ValidationMng.instance(config,validator)
			Some(context.actorOf(props,validator.validation_description+"_val"))
		} else
			None
	}

}


object MeansEndMng {
	def instance(config:ApplicationConfig) : Props = Props.create(classOf[MeansEndMng],config)
}
