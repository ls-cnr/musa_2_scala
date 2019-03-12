package org.icar.musa.actor_model

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import org.icar.fol.AssumptionSet
import org.icar.musa.main_entity.{ConcreteCapabilityFactory, GroundedAbstractCapability}

class Provider_Actor (factory : ConcreteCapabilityFactory,
                      val my_abstract : GroundedAbstractCapability,
                      val assumption : AssumptionSet) extends Actor with ActorLogging {
  case class RegisterInMarketplace_Goal()

  var instance_counter = 0
  var active_workers: List[ActorRef] = List[ActorRef]()

  override def preStart : Unit = {
    log.debug("ready for "+factory.getAbstractName)

    self ! RegisterInMarketplace_Goal()
  }

  override def receive : Receive = {
    case RegisterInMarketplace_Goal() =>
      context.system.eventStream.subscribe(self,classOf[CallForProviders])

    case CallForProviders(abs,requestor) =>
      if (my_abstract.name==abs)
        requestor ! ProviderResponse(abs,self)

    case SelectedForAbstractCapability(abs,employer) =>
      val worker_actor: ActorRef = instantiate_worker(employer)
      sender ! WorkerInstance(abs,worker_actor)
  }

  private def instantiate_worker(recruiter:ActorRef) : ActorRef = {
    val my_concrete = factory.getInstance
    val worker_prop = Props.create(classOf[Worker_Actor], my_concrete, assumption,recruiter)
    val worker_actor: ActorRef = context.actorOf(worker_prop, "wk_" + my_concrete.name+"_"+instance_counter)

    active_workers = worker_actor :: active_workers
    instance_counter += 1

    worker_actor
  }
}


