package org.icar.actor_model

import akka.actor.{ActorRef, Props}
import org.icar.actor_model.protocol.GoalProtocol.InformGoalListChanged
import org.icar.actor_model.protocol._
import org.icar.pmr_solver.high_level_specification.{HL_LTLFormula, LTLGoalSet}
import org.icar.pmr_solver.symbolic_level.{HL2Raw_Map, RawState}

import scala.org.icar.high_level_specification.Solution

class AdaptationMng(config:ApplicationConfig) extends MUSAActor {
	var goal_set : Set[HL_LTLFormula] = Set.empty

	/* context */
	var current_state : Option[RawState] = None
	var current_R2S : Float = 0

	/* abstract solutions */
	var available_solutions : List[Solution] = List()
	var not_available_solutions : List[Solution] = List()
	var opt_the_solution : Option[Solution] = None

	var session_state = Self.init

	/* child */
	val context_actor = init_context_actor
	var meansend_actor = init_meansend_actor
	var orchestrator_actor = init_orchestrator_actor

	object Self {
		def init : State = new UnspecifiedGoals

		trait State {
			def context_changes : State = this
			def goals_change : State = {
				if (goal_set.isEmpty)
					new UnspecifiedGoals()
				else
					new UnknownContext()
			}
			def goal_violation : Self.State = {
				val over_th = check_overcome_violation_tolerance
				if (over_th==true)
					new AdaptationError()
				else
					this
			}
			def has_solutions : State = this
			def no_solutions : State = this
			def solution_applied : State = this
			def unrecoverable_local_failure : State = this
			def solution_becomes_unvalid : State = this
		}

		class UnspecifiedGoals() extends Self.State {
			override def goals_change : Self.State = new UnknownContext()
		}

		class UnknownContext() extends Self.State {
			override def context_changes: Self.State = {
				if (current_R2S==0)
					new FulfilledGoals()
				else
					new PartialSatisfaction()
			}
			override def goal_violation: Self.State = new PartialSatisfaction()
		}

		class PartialSatisfaction() extends Self.State {
			activate_pmr

			override def has_solutions: Self.State = {
				terminate_pmr
				activate_orchestration
				new Recovering()
			}
			override def no_solutions: Self.State = {
				terminate_pmr
				new UnknownContext()
			}
		}

		class FulfilledGoals() extends Self.State {
			override def context_changes: Self.State = {
				if (current_R2S==0)
					this
				else
					new PartialSatisfaction()
			}
			override def goal_violation: Self.State = {
				val over_th = check_overcome_violation_tolerance
				if (over_th==true)
					new AdaptationError()
				else
					new PartialSatisfaction()
			}
		}

		class Recovering() extends Self.State {
			activate_orchestration

			override def context_changes: Self.State = {
				if (current_R2S==0){
					terminate_current_orchestration
					new FulfilledGoals()
				}else
					this
			}
			override def unrecoverable_local_failure: Self.State = {
				terminate_current_orchestration
				new PartialSatisfaction()
			}
			override def solution_becomes_unvalid: Self.State = {
				terminate_current_orchestration
				new PartialSatisfaction()
			}
			override def solution_applied: Self.State = {
				terminate_current_orchestration
				if (current_R2S==0)
					new FulfilledGoals()
				else
					new PartialSatisfaction()
			}
		}

		class AdaptationError() extends Self.State
	}


	override def receive: Receive = {

		case msg:InjectionProtocol.DelegateGoalInjection =>
			merge_goals(msg.goal_model.goals)
			session_state.goals_change

		case msg:RetreatProtocol.DelegateGoalRetreat =>
			subtract_goals(msg.goal_model.goals)
			session_state.goals_change

		case msg:ContextProtocol.InformContextUpdate =>
			update_context(msg.current,msg.distance)
			session_state.context_changes

		case msg:ContextProtocol.InformInternalUpdate =>
			context_actor ! msg

		case msg:AbstractSolProtocol.InformSolutions =>
			available_solutions = msg.sol.toList
			session_state.has_solutions

		case msg:AbstractSolProtocol.InformEmptySet =>
			available_solutions = List.empty
			session_state.no_solutions

		case msg:AdaptationProtocol.InformGoalViolation =>
			session_state.goal_violation

		case msg:AdaptationProtocol.InformConcreteFailure =>
			session_state.unrecoverable_local_failure

		case msg:AdaptationProtocol.InformSolutionBecameInvalid =>
			session_state.solution_becomes_unvalid

		case _=>
	}

	private def check_overcome_violation_tolerance : Boolean = {
		false   //todo implement a strategy for violation tolerance
	}

	def merge_goals(goals: Array[HL_LTLFormula]) = {
		goal_set = goal_set ++ goals

		context_actor ! GoalProtocol.init(goal_set.toArray)
	}
	def subtract_goals(goals: Array[HL_LTLFormula]) = {
		goal_set = goal_set -- goals

		context_actor ! GoalProtocol.init(goal_set.toArray)
	}
	def update_context(current: RawState, distance: Float) : Unit = {
		current_state = Some(current)
		current_R2S = distance
		orchestrator_actor ! ContextProtocol.context_update(current,distance)
	}

	private def activate_pmr : Unit = {
		if (goal_set.nonEmpty) {
			val goal_model = LTLGoalSet(goal_set.toArray)
			meansend_actor ! AbstractSolProtocol.init(current_state.get,goal_model)
		}
	}
	private def terminate_pmr : Unit = {
		//todo
	}

	private def activate_orchestration : Unit = {
		pick_the_solution
		if (opt_the_solution.isDefined)
			orchestrator_actor ! OrchestrationProtocol.init(opt_the_solution.get)
		else
			session_state = new Self.UnknownContext
	}
	private def terminate_current_orchestration : Unit = {
		// todo
	}

	private def pick_the_solution : Unit = {
		// todo
	}

	private def init_context_actor: ActorRef = {
		val props = ContextMng.instance(config)
		context.actorOf(props)
	}

	private def init_meansend_actor: ActorRef = {
		val props = Props.create(classOf[MeansEndMng],config)
		context.actorOf(props)
	}

	private def init_orchestrator_actor: ActorRef = {
		val props = Props.create(classOf[OrchestrationMng],config)
		context.actorOf(props)
	}
}


object AdaptationMng {
	def instance(config:ApplicationConfig) : Props = Props.create(classOf[AdaptationMng],config)
}