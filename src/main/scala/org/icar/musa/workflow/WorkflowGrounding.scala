package org.icar.musa.workflow

import akka.actor.ActorRef
import org.icar.musa.spec.ConcreteCapability

class WorkflowGrounding {
  var mapping : Map[String,(ConcreteCapability,ActorRef)] = Map()

}
