package org.icar.actor_model

import akka.actor.Props

class WorkMng(config:ApplicationConfig,concrete:ConcreteCapability) extends MUSAActor {
	override def receive: Receive = ???
}


object WorkMng {
	def instance(config:ApplicationConfig,concrete:ConcreteCapability) : Props = Props.create(classOf[WorkMng],config, concrete)

}
