package org.icar.ltl.supervisor

import org.icar.fol.GroundPredicate
import org.icar.ltl.LogicAtom
import org.icar.petrinet._


abstract class HNode(val name: String)

abstract class PNNode(override val name: String, val subnodes : Array[HNode]) extends HNode(name) {
  def initial_petrinet() : Petrinet
}

abstract class LogicNode(override val name: String) extends HNode(name)
case class AndNode(override val name: String,subnodes : Array[HNode]) extends LogicNode(name)
case class OrNode(override val name: String,subnodes : Array[HNode]) extends LogicNode(name)
case class NotNode(override val name: String, subnode : HNode) extends LogicNode(name)
case class ImplyNode(override val name: String, left : HNode,right : HNode) extends LogicNode(name)
case class BidImplyNode(override val name: String, left : HNode,right : HNode) extends LogicNode(name)

case class ConditionNode(override val name: String,atom : LogicAtom) extends HNode(name)



/* LTL patterns */
case class GloballyNode(override val name: String, node : HNode)
  extends PNNode(name=name, subnodes = Array(node)) {

  def initial_petrinet() : Petrinet = {
    val start = AnnotatedPlace("start",WaitAcceptedState)
    val end = AnnotatedPlace("end",ErrorState)
    val t1 = AnnotatedInverseTransition(node.name)
    val arc1 = PlaceToTransition(start,t1)
    val arc2 = TransitionToPlace(t1,end)
    val s = PetrinetStructure(Array(start,end),Array(t1),Array(arc1,arc2))
    val d = PetrinetDynamic(scala.collection.mutable.Map("start"->1))
    Petrinet(s,d)
  }
}

case class FinallyNode(override val name: String, node : HNode)
  extends PNNode(name=name, subnodes = Array(node)) {

  def initial_petrinet() : Petrinet = {
    val start = AnnotatedPlace("start",WaitErrorState)
    val end = AnnotatedPlace("end", AcceptedState)
    val t1 = AnnotatedDirectTransition(node.name)
    val arc1 = PlaceToTransition(start,t1)
    val arc2 = TransitionToPlace(t1,end)
    val s = PetrinetStructure(Array(start,end),Array(t1),Array(arc1,arc2))
    val d = PetrinetDynamic(scala.collection.mutable.Map("start"->1))
    Petrinet(s,d)
  }
}

case class NextNode(override val name: String, node : HNode)
  extends PNNode(name=name, subnodes = Array( node,ConditionNode("always",LogicAtom(GroundPredicate("true"))) )) {


  def initial_petrinet() : Petrinet = {
    val start = AnnotatedPlace("start",WaitErrorState)
    val middle = AnnotatedPlace("middle",WaitErrorState)
    val end1 = AnnotatedPlace("e1",AcceptedState)
    val end2 = AnnotatedPlace("e2",ErrorState)
    val t1 = AnnotatedTruthTransition()
    val t2 = AnnotatedDirectTransition(node.name)
    val t3 = AnnotatedInverseTransition(node.name)
    val arc1 = PlaceToTransition(start,t1)
    val arc2 = TransitionToPlace(t1,middle)
    val arc3 = PlaceToTransition(middle,t2)
    val arc4 = PlaceToTransition(middle,t3)
    val arc5 = TransitionToPlace(t2,end1)
    val arc6 = TransitionToPlace(t3,end2)

    val s = PetrinetStructure(Array(start,middle,end1,end2),Array(t1,t2,t3),Array(arc1,arc2,arc3,arc4,arc5,arc6))
    val d = PetrinetDynamic(scala.collection.mutable.Map("start"->1))
    Petrinet(s,d)
  }
}

case class UntilNode(override val name: String, a : HNode, b : HNode)
  extends PNNode(name=name, subnodes = Array( AndNode("sub-ok",Array( NotNode("not-a",a),b ) ),AndNode("sub-err1",Array(a,b) ), AndNode("sub-err2",Array(NotNode("not-a",a),NotNode("not-b",b)) )) ) {


  def initial_petrinet() : Petrinet = {
    val start = AnnotatedPlace("start",WaitErrorState)
    val end1 = AnnotatedPlace("e1",AcceptedState)
    val end2 = AnnotatedPlace("e2",ErrorState)
    val end3 = AnnotatedPlace("e3",ErrorState)
    val t1 = AnnotatedDirectTransition("sub-ok")
    val t2 = AnnotatedDirectTransition("sub-err1")
    val t3 = AnnotatedDirectTransition("sub-err2")
    val arc1 = PlaceToTransition(start,t1)
    val arc2 = PlaceToTransition(start,t2)
    val arc3 = PlaceToTransition(start,t3)
    val arc4 = TransitionToPlace(t1,end1)
    val arc5 = TransitionToPlace(t2,end2)
    val arc6 = TransitionToPlace(t3,end3)
    val s = PetrinetStructure(Array(start,end1,end2,end3),Array(t1,t2,t3),Array(arc1,arc2,arc3,arc4,arc5,arc6))
    val d = PetrinetDynamic(scala.collection.mutable.Map("start"->1))
    Petrinet(s,d)
  }
}

case class ReleaseNode(override val name: String, leftnode : HNode, rightnode : HNode)
  extends PNNode(name=name, subnodes = Array(rightnode,AndNode("sub-and",Array(leftnode,rightnode) )) ) {


  def initial_petrinet() : Petrinet = {
    val start = AnnotatedPlace("start",WaitAcceptedState)
    val end1 = AnnotatedPlace("e1",AcceptedState)
    val end2 = AnnotatedPlace("e2",ErrorState)
    val t1 = AnnotatedDirectTransition("sub-and")
    val t2 = AnnotatedInverseTransition(rightnode.name)
    val arc1 = PlaceToTransition(start,t1)
    val arc2 = PlaceToTransition(start,t2)
    val arc3 = TransitionToPlace(t1,end1)
    val arc4 = TransitionToPlace(t1,end2)
    val s = PetrinetStructure(Array(start,end1,end2),Array(t1,t2),Array(arc1,arc2,arc3,arc4))
    val d = PetrinetDynamic(scala.collection.mutable.Map("start"->1))
    Petrinet(s,d)
  }
}
