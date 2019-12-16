package org.icar.actor_model.role

import akka.actor.ActorRef
import org.icar.actor_model.protocol.ContextProtocol
import org.icar.actor_model.protocol.ContextProtocol._
import org.icar.actor_model.{MUSARole, ProtocolPart}
import org.icar.pmr_solver.high_level_specification.HL_GroundLiteral
import org.icar.pmr_solver.symbolic_level.{RawState, RawVar}

/* CONTEXT UPDATES*/
trait ContextProducerRole extends MUSARole {
	var registered_to_context_updates : List[(ActorRef,ContextRegistration)] = List.empty

	def received_context_registration(sender:ActorRef, msg:ContextRegistration):Unit = {
		registered_to_context_updates = (sender,msg) :: registered_to_context_updates
	}

	def notify_subscribers_of_context_updates(current:RawState, distance:Float):Unit = {
		for (record <- registered_to_context_updates) {
			val actor = record._1
			val msg = record._2
			actor ! msg.context_update(current,distance)
		}
	}

	override def role_description: Receive = {
		case msg:ContextRegistration =>
			received_context_registration(sender,msg)
	}
}

trait ContextConsumerRole extends MUSARole {
	def register_to_context:ProtocolPart = ContextProtocol.init_context_registration

	def received_context_update(sender:ActorRef, msg:InformContextUpdate) : Unit

	override def role_description: Receive = {
		case msg:InformContextUpdate =>
			received_context_update(sender,msg)
	}
}

trait ContextUpdateForwarderRole extends ContextConsumerRole with ContextProducerRole {
	def context_has_changed(current: RawState, distance: Float)
	def received_context_update(sender:ActorRef, msg:InformContextUpdate) : Unit = {
		context_has_changed(msg.current,msg.distance)
		notify_subscribers_of_context_updates(msg.current,msg.distance)
	}
}



/* INTERNAL UPDATES */
trait InternalUpdateProducerRole extends MUSARole {
	var registered_to_internal_updates : List[(ActorRef,ContextRegistration)] = List.empty

	def received_internal_registration(sender:ActorRef, msg:ContextRegistration):Unit = {
		registered_to_internal_updates = (sender,msg) :: registered_to_internal_updates
	}

	def notify_subscribers_of_internal_updates(log:RawVar):Unit = {
		for (record <- registered_to_internal_updates) {
			val actor = record._1
			val msg = record._2
			actor ! msg.internal_update(log)
		}
	}

	override def role_description: Receive = {
		case msg:ContextRegistration =>
			received_internal_registration(sender,msg)
	}
}

trait InternalUpdateConsumerRole extends MUSARole {
	def register_to_internal_producer:ProtocolPart = ContextProtocol.init_context_registration

	def received_internal_update(sender:ActorRef, msg:InformInternalUpdate) : Unit

	override def role_description: Receive = {
		case msg:InformInternalUpdate =>
			received_internal_update(sender,msg)
	}
}

trait InternalUpdateForwarderRole extends InternalUpdateConsumerRole with InternalUpdateProducerRole {
	def internal_has_changed(log:RawVar)
	def received_internal_update(sender:ActorRef, msg:InformInternalUpdate) : Unit = {
		internal_has_changed(msg.log)
		notify_subscribers_of_internal_updates(msg.log)
	}
}



/* OBSERVATION UPDATES */
trait ObservationProducerRole extends MUSARole {
	var registered_to_observation_updates : List[(ActorRef,ObservationRegistration)] = List.empty

	def received_observation_registration(sender:ActorRef, msg:ObservationRegistration):Unit = {
		registered_to_observation_updates = (sender,msg) :: registered_to_observation_updates
	}

	def notify_subscribers_of_observation(preds:List[HL_GroundLiteral]):Unit = {
		for (record <- registered_to_observation_updates) {
			val actor = record._1
			val msg = record._2
			actor ! msg.observation(preds)
		}
	}


	override def role_description: Receive = {
		case msg:ObservationRegistration =>
			received_observation_registration(sender,msg)
	}
}

trait ObservationConsumerRole extends MUSARole {
	def register_to_observer:ProtocolPart = ContextProtocol.init_observation_registration

	def received_observation_update(sender:ActorRef, msg:InformObservations) : Unit

	override def role_description: Receive = {
		case msg:InformObservations =>
			received_observation_update(sender,msg)
	}
}