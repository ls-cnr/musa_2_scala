package org.icar.musa.actor

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import org.icar.fol.AssumptionSet
import org.icar.musa.spec.{ConcreteCapability, GroundedAbstractCapability}


class ProviderActor(factory : GroundedAbstractCapability => ConcreteCapability, val my_abstract : GroundedAbstractCapability, val assumption : AssumptionSet) extends Actor with ActorLogging {
  var instance_counter = 0

  init

  def init : Unit = {
    log.info("ready")

    self ! "register_in_marketplace"

  }

  override def receive: Receive = {
    case "register_in_marketplace" =>
      context.system.eventStream.subscribe(self,classOf[CanProviderDoAbstractCapability])

    case CanProviderDoAbstractCapability(abs,requestor) =>
      //log.info("received call")
      if (my_abstract.name==abs)
        requestor ! ProviderResponse(abs,self)

    case ProviderHasBeenSelectedForAbstractCapability(abs,employer) =>
      //log.info("creating worker")
      val worker_actor: ActorRef = instantiate_worker(employer)
      sender ! WorkerInstanceForEmployer(abs,worker_actor)
  }




  private def instantiate_worker(recruiter:ActorRef) : ActorRef = {
    val my_concrete = factory(my_abstract)
    val worker_prop = Props.create(classOf[WorkerActor], my_concrete, assumption,recruiter)
    val worker_actor: ActorRef = context.actorOf(worker_prop, "wk_" + my_concrete.name+"_"+instance_counter)

    instance_counter += 1

    worker_actor
  }


}


/* OLD
class ProviderActor(val repo : Array[AbstractCapability]) extends Actor with ActorLogging {
  var my_capability : ConcreteCapability = _
  val CorrelationId = 42

  init

  def init : Unit = {
    log.info("ready")

    val abstract1 = recover_abstract("check_wake_up",repo)
    if (abstract1.isDefined) {
      my_capability = new CheckWakeUp1(abstract1.get)
      self ! "register_in_marketplace"
    }

  }

  override def receive: Receive = {
    case "register_in_marketplace" =>
      val sel = context.actorSelection("../marketplace")
      sel ! Identify(CorrelationId)

    case ActorIdentity( CorrelationId , Some(ref)) =>
      log.info("market place is "+ref)
      register_in_market_place(ref)

    case ActorIdentity(_, None) =>
      log.info("no market place exists")

      val system = ActorSystem("MUSA")
      import system.dispatcher
      system.scheduler.scheduleOnce(1000 milliseconds, self, "identify_marketplace")
  }


  def register_in_market_place(mk:ActorRef) : Unit = {
    mk ! RegisterProvider(self,my_capability.abs_cap.name)
  }

  def recover_abstract(str: String, repository: Array[AbstractCapability]): Option[GroundedAbstractCapability] = {
    var cap : Option[GroundedAbstractCapability] = None

    for (c <- repository if c.name==str)
      cap = Some(c.asInstanceOf[GroundedAbstractCapability])

    cap
  }

}

 */