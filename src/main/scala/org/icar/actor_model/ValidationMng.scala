package org.icar.actor_model

import akka.actor.Props
import org.icar.actor_model.protocol.AbstractSolProtocol

import scala.collection.mutable
import scala.concurrent.duration._
import scala.org.icar.high_level_specification.Solution

class ValidationMng(config: ApplicationConfig, validator: SolValidator) extends MUSAActor {
	val sol_queue = new mutable.Queue[AbstractSolProtocol.RequestToValidatePlans]()

	var elaborating_list : List[Solution] = List.empty
	var elaborating_msg : Option[AbstractSolProtocol.RequestToValidatePlans] = None

	var map : List[MetaSolInfo] = List.empty

	system.scheduler.scheduleOnce(config.monitor_delay, self, Self.ValidateNext(0) )

	object Self extends Protocol {
		case class ValidateNext(id:Long) extends ProtocolPart {
			def next_event : ProtocolPart = this
		}
	}


	override def receive: Receive = {
		case event@Self.ValidateNext(_) =>
			if (elaborating_list.isEmpty && !sol_queue.isEmpty) {
					elaborating_msg = Some(sol_queue.dequeue())
					elaborating_list = elaborating_msg.get.sol.toList
			}

			if (!elaborating_list.isEmpty) {
				val sol = elaborating_list.head
				elaborating_list = elaborating_list.tail
				validate(sol)

				if (elaborating_list.isEmpty) {
					pack_and_send(elaborating_msg.get)
					elaborating_msg = None
				}

				system.scheduler.scheduleOnce(10 milliseconds, self, event.next_event )
			} else {

				system.scheduler.scheduleOnce(100 milliseconds, self, event.next_event )
			}



		case msg@AbstractSolProtocol.RequestToValidatePlans(_,reply_to,initial_state,solutions) =>
			sol_queue.enqueue( msg )

	}

	private def validate(solution: Solution) : Unit = {
		val meta_info = validator.validate(solution)
		if (meta_info.valid)
			map = meta_info :: map
	}

	private def pack_and_send(original_msg: AbstractSolProtocol.RequestToValidatePlans) = {
		original_msg.reply_to ! original_msg.validated(map.toArray)
		map = List.empty
	}
}

object ValidationMng {
	def instance(config: ApplicationConfig, validator: SolValidator): Props = Props.create(classOf[ValidationMng],config,validator)

}
