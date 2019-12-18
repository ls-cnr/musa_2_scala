package org.icar.actor_model

import akka.actor.{ActorRef, Props}
import org.icar.actor_model.core.{ApplicationConfig, MUSAActor, MUSALogger, MetaSolInfo, Protocol, ProtocolPart, SolValidator}
import org.icar.actor_model.protocol.AbstractSolProtocol
import org.icar.actor_model.protocol.AbstractSolProtocol.RequestToValidatePlans
import org.icar.actor_model.role.SolutionValidator

import scala.collection.mutable
import scala.concurrent.duration._
import scala.org.icar.high_level_specification.Solution

class ValidationMng(config: ApplicationConfig, validator: SolValidator) extends MUSAActor
	with SolutionValidator {

	val my_log_area = config.logfactory.register_actor(self.path.name)
	mylog("welcome to the MeansEndMng !")

	var elaborating_list : List[Solution] = List.empty
	var elaborating_msg : Option[AbstractSolProtocol.RequestToValidatePlans] = None

	var validated : List[MetaSolInfo] = List.empty

	override def preStart(): Unit = {
		system.scheduler.scheduleOnce(config.monitor_delay, self, Self.ValidateNext(0) )
	}

	object Self extends Protocol {
		case class ValidateNext(id:Long) extends ProtocolPart {
			def next_event : ProtocolPart = this
		}

		def internal_role : Receive = {
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
		}
	}

	override def role__received_request_for_validation(sender: ActorRef, msg: RequestToValidatePlans): Unit = {}


	private def validate(solution: Solution) : Unit = {
		val meta_info = validator.validate(solution)
		if (meta_info.valid)
			validated = meta_info :: validated
	}

	private def pack_and_send(original_msg: RequestToValidatePlans) = {
		if (validated.nonEmpty)
			original_msg.reply_to ! reply_solutions(original_msg,validated.toArray)
		else
			original_msg.reply_to ! reply_no_solutions(original_msg)

		validated = List.empty
	}

}

object ValidationMng {
	def instance(config: ApplicationConfig, validator: SolValidator): Props = Props.create(classOf[ValidationMng],config,validator)

}
