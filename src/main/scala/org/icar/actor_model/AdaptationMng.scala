package org.icar.actor_model

import akka.actor.{ActorRef, Props}
import org.icar.actor_model.core.{ApplicationConfig, MUSAActor, MetaSolInfo}
import org.icar.actor_model.protocol._
import org.icar.actor_model.role._
import org.icar.pmr_solver.high_level_specification.{HL_LTLFormula, LTLGoalSet}
import org.icar.pmr_solver.symbolic_level.{RawState, RawVar}

import scala.org.icar.high_level_specification.Solution

class AdaptationMng(config:ApplicationConfig) extends MUSAActor
	with GoalInjectionConsumerRole
	with GoalRetreatConsumerRole
	with GoalChangeProducerRole
	with ContextUpdateForwarderRole
	with InternalUpdateForwarderRole
	with SolutionCustomer  {

	mylog("welcome to the AdaptationMng !")

	/* child */
	val context_actor = init_context_actor
	var meansend_actor = init_meansend_actor
	var orchestrator_actor = init_orchestrator_actor

	/* current specification */
	var goal_set : Set[HL_LTLFormula] = Set.empty

	/* context */
	var current_state : Option[RawState] = None
	var current_R2S : Float = 0

	/* abstract solutions */
	var available_solutions : List[MetaSolInfo] = List()
	var failed_solutions : List[MetaSolInfo] = List()
	var opt_the_solution : Option[Solution] = None

	var session_state = Self.init

	override def preStart(): Unit = {
		context_actor ! register_to_context
		orchestrator_actor ! register_to_internal_producer
	}

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
			mylog("state=UnspecifiedGoals")
			override def goals_change : Self.State = new UnknownContext()
		}

		class UnknownContext() extends Self.State {
			mylog("state=UnknownContext")
			override def context_changes: Self.State = {
				if (current_R2S==0)
					new FulfilledGoals()
				else
					new PartialSatisfaction()
			}
			override def goal_violation: Self.State = new PartialSatisfaction()
		}

		class PartialSatisfaction() extends Self.State {
			mylog("state=PartialSatisfaction")
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
			mylog("state=FulfilledGoals")
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
			mylog("state=Recovering")
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

		class AdaptationError() extends Self.State {
			mylog("state=AdaptationError")
		}
	}


/*
		case msg:AdaptationProtocol.InformConcreteFailure =>
			session_state.unrecoverable_local_failure

		case msg:AdaptationProtocol.InformSolutionBecameInvalid =>
			session_state.solution_becomes_unvalid

		case _=>
	}
*/

	override def role__react_to_goal_injection(sender: ActorRef, msg: InjectionProtocol.RequestGoalInjection): Unit = {
		val res=merge_goals(msg.goal_model.goals)
		if (res)
			reply_with_success(sender,msg)
		else
			reply_with_failure(sender,msg)
	}

	override def role__received_goal_retreat(sender: ActorRef, msg: RetreatProtocol.RequestGoalRetreat): Unit = {
		val res=subtract_goals(msg.goal_model.goals)
		if (res)
			reply_with_success(sender,msg)
		else
			reply_with_failure(sender,msg)
	}

	override def role__context_has_changed(current: RawState, distance: Float): Unit = {
		current_state = Some(current)
		current_R2S = distance
		session_state = session_state.context_changes
	}
	override def role__internal_has_changed(log: RawVar): Unit = {} //only forward behavior **encapsulated into role**

	override def role__received_abstract_solutions(sender: ActorRef, msg: AbstractSolProtocol.InformSolutions): Unit = {
		available_solutions = msg.sol.toList
		session_state = session_state.has_solutions
	}

	override def role__received_empty_solutions(sender: ActorRef, msg: AbstractSolProtocol.InformEmptySet): Unit = {
		available_solutions = List.empty
		session_state = session_state.no_solutions
	}

	override def role__received_goal_violation(sender: ActorRef, msg: ContextProtocol.InformGoalViolation): Unit = {
		session_state = session_state.goal_violation
	}



	private def check_overcome_violation_tolerance : Boolean = {
		false   //todo implement a strategy for violation tolerance
	}

	private def merge_goals(goals: Array[HL_LTLFormula]):Boolean = {
		goal_set = goal_set ++ goals
		session_state = session_state.goals_change

		notify_subscribers_of_goalchanges(goal_set.toArray)
		true
	}
	private def subtract_goals(goals: Array[HL_LTLFormula]):Boolean = {
		goal_set = goal_set -- goals
		session_state = session_state.goals_change
		notify_subscribers_of_goalchanges(goal_set.toArray)
		true
	}

	private def activate_pmr : Unit = {
		if (goal_set.nonEmpty) {
			val goal_model = LTLGoalSet(goal_set.toArray)
			meansend_actor ! msg_to_request_solutions_for_problem(current_state.get,goal_model)
		}
	}
	private def terminate_pmr : Unit = {
		//todo
	}

	private def activate_orchestration : Unit = {
		pick_the_solution
		//todo decomment
		/*
		if (opt_the_solution.isDefined)
			orchestrator_actor ! OrchestrationProtocol.init(opt_the_solution.get)
		else
			session_state = new Self.UnknownContext
*/
	}

	private def terminate_current_orchestration : Unit = {
		// todo
	}

	private def pick_the_solution : Unit = {
		// todo
	}

	private def init_context_actor : ActorRef = {
		val props = ContextMng.instance(config)
		context.actorOf(props,"ContextMng")
	}

	private def init_meansend_actor : ActorRef = {
		val props = MeansEndMng.instance(config)
		context.actorOf(props,"MeansEndMng")
	}

	private def init_orchestrator_actor: ActorRef = {
		val props = Props.create(classOf[OrchestrationMng],config)
		context.actorOf(props,"OrchestrMng")
	}

}


object AdaptationMng {
	def instance(config:ApplicationConfig) : Props = Props.create(classOf[AdaptationMng],config)
}
