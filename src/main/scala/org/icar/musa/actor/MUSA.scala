package org.icar.musa.actor

import akka.actor.{ActorSystem, Props}
import org.icar.musa.scenarios.UPA4SAR.UPA4SAR_spec_loader
import org.icar.musa.scenarios.sps.SPSSpecLoader
import org.icar.musa.spec._


object MUSA extends App {
  //val url = "jdbc:mysql://aose.pa.icar.cnr.it:3306/musa_db"
  //val driver = "com.mysql.jdbc.Driver"
  //val musa_db = DBInfo(url,driver,"aose","aose")

  val UPAspec_loader = new UPA4SAR_spec_loader("/Users/luca/workspace-scala/musa_2/prin_data")

  val sps_spec_loader = new SPSSpecLoader("./sps_data")

  start_musa(sps_spec_loader)


  def start_musa(spec_loader : SpecificationLoader) : Unit = {
    val system : ActorSystem = ActorSystem("MUSA")
    val props = Props.create(classOf[MUSAActor],spec_loader)
    system.actorOf(props, "root")
  }




}
