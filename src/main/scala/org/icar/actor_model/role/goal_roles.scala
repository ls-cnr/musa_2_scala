package org.icar.actor_model.role

import akka.actor.ActorRef
import org.icar.actor_model.core.{MUSARole, ProtocolPart}
import org.icar.actor_model.protocol.GoalProtocol
import org.icar.actor_model.protocol.GoalProtocol.{GoalListRegistration, InformGoalListChanged}
import org.icar.actor_model.protocol.InjectionProtocol.RequestGoalInjection
import org.icar.actor_model.protocol.RetreatProtocol.RequestGoalRetreat
import org.icar.pmr_solver.high_level_specification.HL_LTLFormula

/* GOAL INJECTION\RETREAT */
trait GoalInjectionConsumerRole extends MUSARole {
	def role__react_to_goal_injection(sender:ActorRef, msg:RequestGoalInjection):Unit

	def reply_with_success(sender:ActorRef, msg:RequestGoalInjection) =
		sender ! msg.success()

	def reply_with_failure(sender:ActorRef, msg:RequestGoalInjection) =
		sender ! msg.failure()

	registerRole {
		case msg:RequestGoalInjection =>
			mylog("goals injected")
			role__react_to_goal_injection(sender,msg)
	}
}
trait GoalRetreatConsumerRole extends MUSARole {
	def role__received_goal_retreat(sender:ActorRef, msg:RequestGoalRetreat):Unit

	def reply_with_success(sender:ActorRef, msg:RequestGoalRetreat) =
		sender ! msg.success()

	def reply_with_failure(sender:ActorRef, msg:RequestGoalRetreat) =
		sender ! msg.failure()

	registerRole {
		case msg:RequestGoalRetreat =>
			mylog("goals retreated")
			role__received_goal_retreat(sender,msg)
	}
}


/* GOALS CHANGE UPDATES */
trait GoalChangeProducerRole extends MUSARole {
	var registered_to_goal_updates : List[(ActorRef,GoalListRegistration)] = List.empty

	def role__received_context_registration(sender:ActorRef, msg:GoalListRegistration):Unit = {
		registered_to_goal_updates = (sender,msg) :: registered_to_goal_updates
	}

	def notify_subscribers_of_goalchanges(goals:Array[HL_LTLFormula]):Unit = {
		mylog("notify all subscribers of goals change")
		for (record <- registered_to_goal_updates) {
			val actor = record._1
			val msg = record._2
			actor ! msg.goal_update(goals)
		}
	}

	registerRole {
		case msg:GoalListRegistration =>
			mylog("actor "+sender.path.name+" registered to goals change updated")
			role__received_context_registration(sender,msg)
	}
}
trait GoalChangeConsumerRole extends MUSARole {
	def role__register_to_goal_changes:ProtocolPart = GoalProtocol.init

	def received_goals_update(sender:ActorRef, msg:InformGoalListChanged) : Unit

	registerRole {
		case msg:InformGoalListChanged =>
			mylog("goals changed")
			received_goals_update(sender,msg)
	}
}
