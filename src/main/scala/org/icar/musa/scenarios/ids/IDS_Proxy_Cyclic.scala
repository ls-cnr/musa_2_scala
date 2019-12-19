package org.icar.musa.scenarios.ids

import org.icar.fol.{AtomTerm, GroundPredicate}
import org.icar.musa.actor_model.RequestNewSession
import org.icar.musa.context.{DataIn, StateOfWorld}
import org.icar.musa.specification.TimedProxyStrategy

import scala.concurrent.duration._

/* launch a new request every x seconds */
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


