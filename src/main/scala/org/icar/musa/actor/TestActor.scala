package org.icar.musa.actor

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import org.icar.fol.Assumption


object runActor extends App {
  val system : ActorSystem = ActorSystem("actor-system")
  val props = Props.create(classOf[TestActor])
  val my_actor : ActorRef = system.actorOf(props, "test-actor")
}


class TestActor extends Actor {

  println("parent actor")
  val rule : Assumption = Assumption("anomaly(user, temperature) :- not temperature(user, normal).")
  println(rule)

  val props = Props.create(classOf[TestSubActor])
  val my_sub_actor : ActorRef = context.actorOf(props, "sub-actor1")

  val props2 = Props.create(classOf[TestSubActor])
  val my_sub_actor2 : ActorRef = context.actorOf(props2, "sub-actor2")

  override def receive: Receive = {
    case x ⇒ println("received x")
  }


}

class TestSubActor extends Actor {

  println("child actor")
  val rule : Assumption = Assumption("anomaly(user, temperature) :- not temperature(user, normal).")
  println(rule)

  override def receive: Receive = {
    case x ⇒ println("received x")
  }


}
