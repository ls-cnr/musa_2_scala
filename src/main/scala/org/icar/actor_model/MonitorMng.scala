package org.icar.actor_model

import akka.actor.Props
import org.icar.actor_model.core.{ApplicationConfig, EnvObserver, MUSAActor, MUSALogger, Protocol, ProtocolPart}
import org.icar.actor_model.role.ObservationProducerRole


class MonitorMng(config:ApplicationConfig, obs:EnvObserver) extends MUSAActor
	with ObservationProducerRole {

	val my_log_area = config.logfactory.register_actor(self.path.name)
	mylog(s"welcome to the MonitorMng(${obs.variable_description}) !")

	override def preStart(): Unit = {
		registerRole(Self.internal_role)
		system.scheduler.scheduleOnce(config.monitor_delay, self, Self.Observe(0) )
	}

	object Self extends Protocol {
		case class Observe(id:Long) extends ProtocolPart {
			def next : ProtocolPart = this
		}

		def internal_role : Receive = {
			case event@Self.Observe(_) =>
				val pred_list = obs.read_state
				if (pred_list.nonEmpty)
					notify_subscribers_of_observation(pred_list)

				system.scheduler.scheduleOnce(config.monitor_delay, self, event.next )
		}
	}



}


object MonitorMng {
	def instance(config:ApplicationConfig,obs:EnvObserver) : Props = Props.create(classOf[MonitorMng],config, obs)
}
