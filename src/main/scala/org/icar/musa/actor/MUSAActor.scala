package org.icar.musa.actor

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import org.icar.musa.scenarios.UPA4SAR.{AlertAnomaly1, CheckWakeUp1, RemindWakeUp1}
import org.icar.musa.spec.{AbstractCapability, DomainLoader, GroundedAbstractCapability, SpecificationLoader}

class MUSAActor(spec_loader : SpecificationLoader) extends Actor with ActorLogging {

  override def preStart : Unit = {
    log.info("ready")

    for_each_domain_create_actor

  }


  override def receive: Receive = {
    case _ ⇒ println("welcome in MUSA")
  }


  private def for_each_domain_create_actor : Unit = {
    for (d <- spec_loader.domains if d.active==true) {
      start_providers(d)

      val props = Props.create(classOf[DomainActor],d)
      val actor_name = d.name.replace(' ', '_').toLowerCase
      val domain_actor : ActorRef = context.actorOf(props, actor_name)

    }

    def start_providers(domain : DomainLoader) : Unit = {
      val abs_rep = domain.abstract_repository
      val factories = domain.concrete_repository
      var counter = 1

      for (f <- factories) {

        val abs_cap1 = recover_abstract(f.getAbstractName,abs_rep)
        if (abs_cap1.isDefined) {
          val props1 = Props.create(classOf[ProviderActor],f,abs_cap1.get,domain.assumption)
          context.actorOf(props1,"provider"+counter)
          counter += 1
        }

      }
    }

    /*def start_providers(domain : DomainLoader) : Unit = {
      if (domain.name=="WakeUp") {
        val abs_cap1 = recover_abstract("check_wake_up",domain.abstract_repository)
        if (abs_cap1.isDefined) {
          val props1 = Props.create(classOf[ProviderActor],(x:GroundedAbstractCapability)=>new CheckWakeUp1(x),abs_cap1.get,domain.assumption)
          context.actorOf(props1,"provider1")
        }

        val abs_cap2 = recover_abstract("remind_wake_up",domain.abstract_repository)
        if (abs_cap2.isDefined) {
          val props2 = Props.create(classOf[ProviderActor],(x:GroundedAbstractCapability)=>new RemindWakeUp1(x),abs_cap2.get,domain.assumption)
          context.actorOf(props2,"provider2")
        }

        val abs_cap3 = recover_abstract("alert_anomaly",domain.abstract_repository)
        if (abs_cap3.isDefined) {
          val props3 = Props.create(classOf[ProviderActor], (x: GroundedAbstractCapability) => new AlertAnomaly1(x), abs_cap3.get, domain.assumption)
          context.actorOf(props3, "provider3")
        }
      }
    }*/

    def recover_abstract(str: String, repository: Array[AbstractCapability]): Option[GroundedAbstractCapability] = {
      var cap : Option[GroundedAbstractCapability] = None

      for (c <- repository if c.name==str)
        cap = Some(c.asInstanceOf[GroundedAbstractCapability])

      cap
    }



    /*import java.sql.{Connection,DriverManager}

    try {
      Class.forName(musa_db.driver)
      var connection:Connection = DriverManager.getConnection(musa_db.url, musa_db.user, musa_db.psw)
      val statement = connection.createStatement
      val rs = statement.executeQuery("SELECT idDomain, name FROM domain")

      if (rs.next) {    //IT WAS NEXT
        val id : Int = rs.getInt("idDomain")
        val name : String = rs.getString("name")

        val props = Props.create(classOf[DomainActor],musa_db, Predef.int2Integer(id))
        val actor_name = name.replace(' ', '_').toLowerCase
        val domain_actor : ActorRef = context.actorOf(props, actor_name)
      }

      connection.close
    } catch {
      case e: Exception => e.printStackTrace
    }*/

  }


}
