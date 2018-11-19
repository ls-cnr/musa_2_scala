package org.icar.musa.actor

import akka.actor.{ActorRef, ActorSystem, Props}
import org.icar.musa.spec.{SpecificationLoader, UPA4SAR_spec_loader}


object MUSA extends App {
  //val url = "jdbc:mysql://aose.pa.icar.cnr.it:3306/musa_db"
  //val driver = "com.mysql.jdbc.Driver"
  //val musa_db = DBInfo(url,driver,"aose","aose")

  val spec_loader = new UPA4SAR_spec_loader

  val system : ActorSystem = ActorSystem("MUSA")
  val props = Props.create(classOf[MUSAActor],spec_loader)
  val musa_actor : ActorRef = system.actorOf(props, "root")
}
