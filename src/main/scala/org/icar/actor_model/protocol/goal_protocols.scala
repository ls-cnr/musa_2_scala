package org.icar.actor_model.protocol

import akka.actor.ActorRef
import org.icar.actor_model.protocol.InjectionProtocol.GoalInjectionProtocolPart
import org.icar.actor_model.{Protocol, ProtocolPart}
import org.icar.pmr_solver.high_level_specification.{HL_LTLFormula, LTLGoalSet}

package object GoalProtocol extends Protocol {
	def init(goals:Array[HL_LTLFormula]):ProtocolPart = InformGoalListChanged(get_id,goals)
	case class InformGoalListChanged private(id:Long,goals:Array[HL_LTLFormula]) extends ProtocolPart
}

package object InjectionProtocol extends Protocol {
	def init(session_id:String, goal_model:LTLGoalSet) : ProtocolPart = RequestGoalInjection(get_id,session_id,goal_model)

	trait GoalInjectionProtocolPart extends ProtocolPart

	case class RequestGoalInjection private(id:Long,session_id:String, goal_model:LTLGoalSet) extends GoalInjectionProtocolPart {
		def forward(sender:ActorRef) : ProtocolPart = DelegateGoalInjection(this.id,sender,goal_model)
		def failure() : ProtocolPart = InformFailure(this.id)
	}
	case class DelegateGoalInjection private(id:Long,sender:ActorRef, goal_model:LTLGoalSet) extends GoalInjectionProtocolPart {
		def success() : ProtocolPart = InformSuccess(this.id)
		def failure() : ProtocolPart = InformFailure(this.id)
	}
	case class InformSuccess private(id:Long) extends GoalInjectionProtocolPart
	case class InformFailure private(id:Long) extends GoalInjectionProtocolPart
}

package object RetreatProtocol extends Protocol {
	def init(session_id:String, goal_model:LTLGoalSet) : ProtocolPart = RequestGoalRetreat(get_id,session_id,goal_model)

	trait GoalRetreatProtocolPart extends ProtocolPart

	case class RequestGoalRetreat private(id:Long,session_id:String, goal_model:LTLGoalSet) extends GoalRetreatProtocolPart {
		def forward(sender:ActorRef) : ProtocolPart = DelegateGoalRetreat(this.id,sender,goal_model)
		def failure() : ProtocolPart = InformFailure(this.id)
	}
	case class DelegateGoalRetreat private(id:Long,sender:ActorRef, goal_model:LTLGoalSet) extends GoalInjectionProtocolPart {
		def success() : ProtocolPart = InformSuccess(this.id)
		def failure() : ProtocolPart = InformFailure(this.id)
	}
	case class InformSuccess private(id:Long) extends GoalRetreatProtocolPart
	case class InformFailure private(id:Long) extends GoalRetreatProtocolPart
}

