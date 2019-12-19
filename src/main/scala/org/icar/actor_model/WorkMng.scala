package org.icar.actor_model

import akka.actor.{ActorRef, Props}
import org.icar.actor_model.core.{ApplicationConfig, ConcreteCapability, MUSAActor}
import org.icar.actor_model.protocol.GroundingProtocol
import org.icar.actor_model.role.GroundingAuctionParticipant
import org.icar.pmr_solver.high_level_specification.ConstantTerm

import scala.org.icar.high_level_specification.Task

class WorkMng(config:ApplicationConfig,concrete:ConcreteCapability) extends MUSAActor
	with GroundingAuctionParticipant {

	val my_log_area = config.logfactory.register_actor(self.path.name)

	var jobs : List[(Task,Boolean)] = List.empty

	override def role__received_call_for_grounding_auction(sender: ActorRef, msg: GroundingProtocol.CallForProposals): Unit = {
		if (matching(msg.task))
			sender ! msg.participate(concrete.capability_description)
	}

	override def role__win_task_auction(sender: ActorRef, msg: GroundingProtocol.AssignTask): Unit = {
		jobs = (msg.task,false) :: jobs
	}


	def grounding_is_compatible(ground: Map[String, ConstantTerm]): Boolean = {
		var ret = true
		for (k<-ground.keys)
			if (!concrete.constraint.contains(k) || ground(k)!=concrete.constraint(k))
				ret=false

		ret
	}


	private def matching(task:Task) : Boolean = {
		var ret =false
		if (task.grounding.c.id == concrete.ref_abstract)
			if (grounding_is_compatible(task.grounding.ground))
				ret=true

		ret
	}

}


object WorkMng {
	def instance(config:ApplicationConfig,concrete:ConcreteCapability) : Props = Props.create(classOf[WorkMng],config, concrete)

}
