package org.icar.musa.actor_model

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import org.icar.musa.specification.{DomainLoader, SpecificationLoader}
import org.icar.musa.main_entity.{AbstractCapability, GroundedAbstractCapability}

class MUSA_Actor (spec_loader : SpecificationLoader) extends Actor with ActorLogging {
  var provider_counter = 1
  var active_domains: List[ActorRef] = List[ActorRef]()

  override def preStart : Unit = {
    log.info("ready")

    start_domains
  }

  override def receive: Receive = {
    case _ ⇒ println("welcome in MUSA")
  }

  private def start_domains() : Unit = {
    for (d <- spec_loader.domains if d.active==true) {
      start_providers(d)

      val props = Props.create(classOf[Domain_Actor],d)
      val actor_name = d.name.replace( ' ', '_').toLowerCase
      val domain_actor : ActorRef = context.actorOf(props, actor_name)

      active_domains = domain_actor :: active_domains
    }
  }

  def start_providers(domain : DomainLoader) : Unit = {
    val abs_rep = domain.abstract_repository
    val factories = domain.concrete_repository

    for (f <- factories) {

      val abs_cap1 = recover_abstract(f.getAbstractName,abs_rep)
      if (abs_cap1.isDefined) {
        val props1 = Props.create(classOf[Provider_Actor],f,abs_cap1.get,domain.assumption)
        context.actorOf(props1,"provider"+provider_counter)
        provider_counter += 1
      }

    }
  }

  def recover_abstract(str: String, repository: Array[AbstractCapability]): Option[GroundedAbstractCapability] = {
    var cap : Option[GroundedAbstractCapability] = None

    for (c <- repository if c.name==str)
      cap = Some(c.asInstanceOf[GroundedAbstractCapability])

    cap
  }


}
