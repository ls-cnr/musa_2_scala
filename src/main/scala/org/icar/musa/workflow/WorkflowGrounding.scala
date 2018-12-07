package org.icar.musa.workflow

import akka.actor.ActorRef
import org.icar.musa.main_entity.ConcreteCapability

class WorkflowGrounding {
  var mapping : Map[String,ActorRef] = Map()

}
