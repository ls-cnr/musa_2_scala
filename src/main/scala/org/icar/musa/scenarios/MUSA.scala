package org.icar.musa.scenarios

import akka.actor.{ActorSystem, Props}
import org.icar.musa.actor.MUSAActor
import org.icar.musa.scenarios.UPA4SAR.{UPA4SAR_domain_loader, UPA4SAR_spec_loader}
import org.icar.musa.scenarios.sps.{SPSScenario1, SPSSpecLoader}
import org.icar.musa.spec._


object MUSA extends App {
  //val url = "jdbc:mysql://aose.pa.icar.cnr.it:3306/musa_db"
  //val driver = "com.mysql.jdbc.Driver"
  //val musa_db = DBInfo(url,driver,"aose","aose")

  val spec_loader = new AllInSpecLoader("./data")

  start_musa(spec_loader)


  def start_musa(spec_loader : SpecificationLoader) : Unit = {
    val system : ActorSystem = ActorSystem("MUSA")
    val props = Props.create(classOf[MUSAActor],spec_loader)
    system.actorOf(props, "root")
  }

}

class AllInSpecLoader(path: String) extends SpecificationLoader {
  override def domains: Array[DomainLoader] = {
    Array(
      new SPSScenario1(path),
      new UPA4SAR_domain_loader(path))
  }
}

