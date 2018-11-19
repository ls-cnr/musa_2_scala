package org.icar.musa.actor

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import org.icar.musa.spec.SpecificationLoader

class MUSAActor(musa_db : SpecificationLoader) extends Actor with ActorLogging {

  init

  private def init : Unit = {
    log.info("ready")

    for_each_domain_create_actor


    //val props = Props.create(classOf[DomainActor],domain)
    //val domain_actor : ActorRef = context.actorOf(props, "WakeUp")

  }


  override def receive: Receive = {
    case x ⇒ println("ricevo x")
  }




  private def for_each_domain_create_actor : Unit = {
    for (d <- musa_db.domains) {
      val props = Props.create(classOf[DomainActor],d)
      val actor_name = d.name.replace(' ', '_').toLowerCase
      val domain_actor : ActorRef = context.actorOf(props, actor_name)

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
