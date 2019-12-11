package org.icar.actor_model

import akka.actor.Props
import org.icar.actor_model.protocol.ContextProtocol


class MonitorMng(config:ApplicationConfig, obs:EnvObserver) extends MUSAActor {

	object Self extends Protocol {
		case class Observe(id:Long) extends ProtocolPart {
			def next : ProtocolPart = this
		}
	}


	override def receive: Receive = {
		case event@Self.Observe(_) =>
			val pred_list = obs.read_state
			if (pred_list.nonEmpty)
				context.parent ! ContextProtocol.observation(pred_list)

			system.scheduler.scheduleOnce(config.monitor_delay, self, event.next )

		case _=>
	}

}


object MonitorMng {
	def instance(config:ApplicationConfig,obs:EnvObserver) : Props = Props.create(classOf[MonitorMng],config, obs)
}
