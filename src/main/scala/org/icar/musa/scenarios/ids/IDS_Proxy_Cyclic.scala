package org.icar.musa.scenarios.ids

import java.io.File

import org.icar.fol.{AtomTerm, GroundPredicate}
import org.icar.musa.actor_model.RequestNewSession
import org.icar.musa.context.{DataIn, StateOfWorld}
import org.icar.musa.specification.{ProxyCapability, TimedProxyStrategy}

import scala.concurrent.duration._

class IDS_Proxy_Cyclic extends TimedProxyStrategy(1 seconds) {
  var doc_id = 1

  override def generate_new_request: RequestNewSession = {
    val d1 = new DataIn()
    d1.registerVariable("document_id",doc_id)
    val w1 = StateOfWorld.create(GroundPredicate("request", AtomTerm("id")),GroundPredicate("document", AtomTerm("id")))

    doc_id += 1

    RequestNewSession(d1,w1)
  }
}



class IDS_Proxy_FileMonitor(path : String) extends ProxyCapability {
  val delay = 1 second
  val folder = new File(path+"/tmp/checkin")
  var already_found = List[String]()

  println(folder.getAbsolutePath)

  def generate_new_request(doc_id : String) : RequestNewSession = {
    val d1 = new DataIn()
    d1.registerVariable("document_id",doc_id)
    val w1 = StateOfWorld.create(GroundPredicate("request", AtomTerm("id")),GroundPredicate("document", AtomTerm("id")))

    RequestNewSession(d1,w1)
  }

  override def run(): Unit = {
    while (folder != null) {
      //println("looking at dir "+folder.getName)
      val files: Array[File] = folder.listFiles()
      for (file <- files) {
        val name = file.getName
        if (name != ".DS_Store" && !already_found.contains(name)) {
          //println("found "+name)
          val request = generate_new_request(name)
          my_callback(request)

          already_found = name :: already_found

        }
      }
      Thread.sleep(delay.toMillis)
    }
  }
}
