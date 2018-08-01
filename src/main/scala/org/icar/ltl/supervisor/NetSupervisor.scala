package org.icar.ltl.supervisor

import org.icar.fol._
import org.icar.ltl._
import org.icar.musa.context.StateOfWorld
import org.icar.petrinet._

case class NetSupervisor(net : NetHierarchy, petrinets: Map[String, Petrinet], current_state: PlaceType, distance_to_satisfaction: Float) { //, current_token: TokenConf) {
  def isExitable: Boolean = {
    current_state == AcceptedState() | current_state == WaitAcceptedState()
  }

  def isAccepted: Boolean = {
    current_state == AcceptedState()
  }

  override def toString: String = "NetSup("+petrinets+","+current_state+","+distance_to_satisfaction+")"

}

object NetSupervisor {

  def apply(net: NetHierarchy, petrinets: scala.collection.Map[String, Petrinet], current_state: PlaceType, distance_to_satisfaction: Float): NetSupervisor = new NetSupervisor(net, petrinets.toMap, current_state, distance_to_satisfaction)

  def init_net_supervisor_1: NetSupervisor = {
    val ass = AssumptionSet(Assumption("document(X) :- attach(X)."))
    val a = GroundPredicate("attach", AtomTerm("doc"))
    val w = StateOfWorld.create(a)


    val f1 = Globally(LogicAtom("document", AtomTerm("doc")))
    val f2 = Finally(LogicAtom("ready", AtomTerm("doc")))
    val f3 = LogicConjunction(f1, f2)

    val builder = new SupervisorBuilder()
    builder.initialize(f3, w, ass)
  }
}

