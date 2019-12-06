package org.icar.actor_model

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import org.icar.pmr_solver.high_level_specification.{AvailableActions, Domain, LTLGoalSet}
import org.icar.pmr_solver.symbolic_level.{HL2Raw_Map, RawState}

class SessionActor(domain:Domain,goal_model:LTLGoalSet,available_actions:AvailableActions) extends Actor with ActorLogging {
	val raw_domain = new HL2Raw_Map(domain)

	val env_actor = init_environment_actor
	var some_configurator_actor : Option[ActorRef] = None
	var some_orchestrator_actor : Option[ActorRef] = None


	override def receive: Receive = {
		case GoalMsg.Injection(id,model) =>


		case GoalMsg.Retreat(id,model) =>


		case WorldMsg.Initial(state,time_stamp) =>
			if (!some_configurator_actor.isDefined)
				some_configurator_actor = Some( init_configurator_actor(state) )

		case PlanningMsg.FullSolutionArray =>
			if (!some_orchestrator_actor.isDefined)
				some_orchestrator_actor = Some( init_orchestrator_actor )

	}

	private def init_environment_actor: ActorRef = {
		val props = Props.create(classOf[WorldActor],domain,goal_model)
		context.actorOf(props)
	}

	private def init_configurator_actor(state:RawState): ActorRef = {
		val props = Props.create(classOf[PMReasonerActor],domain,goal_model,available_actions,state)
		context.actorOf(props)
	}

	private def init_orchestrator_actor: ActorRef = {
		val props = Props.create(classOf[OrchestratorActor],available_actions)
		context.actorOf(props)
	}
}
