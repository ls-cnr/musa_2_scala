package org.icar.musa.actor

import akka.actor.{ActorRef, ActorSystem, Props}


object MUSA extends App {
  val url = "jdbc:mysql://aose.pa.icar.cnr.it:3306/musa_db"
  val driver = "com.mysql.jdbc.Driver"
  val musa_db = DBInfo(url,driver,"aose","aose")

  val system : ActorSystem = ActorSystem("MUSA")
  val props = Props.create(classOf[MUSAActor],musa_db)
  val musa_actor : ActorRef = system.actorOf(props, "root")
}
