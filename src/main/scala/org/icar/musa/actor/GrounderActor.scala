package org.icar.musa.actor

import akka.actor.{Actor, ActorLogging, ActorRef}
import org.icar.musa.actor.concrete.WakeUpConcreteRepository
import org.icar.musa.spec.{ConcreteCapability, GroundedAbstractCapability}
import org.icar.musa.workflow.WorkflowGrounding

class GrounderActor extends Actor with ActorLogging {
  val concrete_repository : Array[ConcreteCapability] = load_concrete_capabilities

  var workflow_grounding : Map[String,ConcreteCapability] = Map()

  init

  private def init : Unit = {
    log.info("ready")
  }

  override def receive: Receive = {

    case AskConcrete(abs_name : String) =>
      if (workflow_grounding.contains(abs_name))
        sender() ! MappingAbstractConcrete(abs_name, workflow_grounding.get(abs_name).get )

      else {
        val selected = for (c <- concrete_repository if c.abs_cap.name == abs_name) yield c
        if (selected.length>0) {
          workflow_grounding += (abs_name -> selected(0))
          sender() ! MappingAbstractConcrete(abs_name,selected(0))
        }

      }

  }


  def load_concrete_capabilities: Array[ConcreteCapability] = {
    val wakeup = new WakeUpConcreteRepository

    wakeup.repository
  }


}


