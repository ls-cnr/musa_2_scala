package org.icar.ltl.supervisor

import org.icar.fol._
import org.icar.musa.context.StateOfWorld
import org.icar.petrinet._

class ResistanceToFullAchievement(val root: HNode, val w: StateOfWorld, val cond : Map[String, Boolean], petrinet_state: String => PlaceType ) {
  val RMAX = 100
  val RINF = 10000
  var r: Float =0

  r = deduce_resistance(root, direct = true)


  private def deduce_resistance(node: HNode, direct : Boolean): Float = {
    node match {
      case ConditionNode(name , atom) => condition_resistance(name,direct)
      case AndNode(_, subnodes) => if (direct) sum_resistances(subnodes,direct) else parallel_of_resistances(subnodes,direct)
      case OrNode(_, subnodes) => if (direct) parallel_of_resistances(subnodes,direct) else sum_resistances(subnodes,direct)
      case NotNode(_, subnode) => deduce_resistance(subnode,!direct)
      case ImplyNode(_, subnode1, subnode2) => resistance_of_imply(subnode1, subnode2, direct)
      case BidImplyNode(_, subnode1, subnode2) => resistance_of_bid_imply(subnode1, subnode2, direct)
      case GloballyNode(name, subnode) => resistance_of_globally(petrinet_state(name),subnode,direct)
      case FinallyNode(name, subnode) => resistance_of_finally(petrinet_state(name),subnode,direct)
      case NextNode(name, subnode) => resistance_of_next(petrinet_state(name),subnode,direct)
      case UntilNode(name, subnode1, subnode2) => resistance_of_until(petrinet_state(name),subnode1,subnode2,direct)
      case ReleaseNode(name, subnode1, subnode2) => resistance_of_release(petrinet_state(name),subnode1,subnode2,direct)
      case _ => RINF
    }
  }

  def resistance_of_until(state: PlaceType, node1: HNode, node2: HNode, direct: Boolean): Float = {
    if (direct) {
      if (state==WaitErrorState) deduce_resistance(node1,!direct)+deduce_resistance(node2,direct)
      else if (state==AcceptedState) 0
      else RINF

    } else {
      if (state==WaitErrorState) deduce_resistance(node1,!direct)+deduce_resistance(node2,!direct)
      else if (state==ErrorState) 0
      else RINF
    }
  }

  def resistance_of_release(state: PlaceType, node1: HNode, node2: HNode, direct: Boolean): Float = {
    if (direct) {
      if (state==WaitAcceptedState) deduce_resistance(node1,direct)+deduce_resistance(node2,direct)
      else if (state==AcceptedState) 0
      else RINF

    } else {
      if (state==WaitAcceptedState) deduce_resistance(node1,direct)+deduce_resistance(node2,!direct)
      else if (state==ErrorState) 0
      else RINF
    }
  }

  def resistance_of_next(state: PlaceType, node: HNode, direct: Boolean): Float = {
    if (direct) {
      if (state==WaitErrorState) deduce_resistance(node,direct)
      else if (state==ErrorState) RINF
      else 0

    } else {
      if (state==WaitErrorState) deduce_resistance(node,!direct)
      else if (state==ErrorState) 0
      else RINF
    }
  }

  def resistance_of_finally(state: PlaceType, node: HNode, direct: Boolean): Float ={
    if (direct) {
      if (state==WaitErrorState) deduce_resistance(node,direct) else 0
    } else {
      if (state==WaitErrorState) 0 else RINF
    }
  }
  def resistance_of_globally(state: PlaceType, node: HNode, direct: Boolean): Float = {
    if (direct) {
      if (state==WaitAcceptedState) 0 else RINF
    } else {
      if (state==WaitAcceptedState) deduce_resistance(node,!direct) else 0
    }
  }

  private def resistance_of_imply(node1: HNode, node2: HNode, direct: Boolean): Float = {
    val r1 = deduce_resistance(node1,direct)
    val r2 = deduce_resistance(node2,direct)
    if (direct) {
      if (r1==0) r2 else 0
    }else{
      if (r2==0) r1 else 0
    }
  }

  private def resistance_of_bid_imply(node1: HNode, node2: HNode, direct: Boolean): Float = {
    val r1 = deduce_resistance(node1,direct)
    val r2 = deduce_resistance(node2,direct)
    if (direct) {
      if (r1 == 0 & r2 == 0) 0
      else if (r1 > 0 & r2 > 0) 0
      else math.max(r1,r2)

    } else {
      if (r1 == 0 & r2 == 0) RMAX
      else if (r1 > 0 & r2 > 0) math.max(r1,r2)
      else 0
    }
  }

  private def condition_resistance(name : String, direct : Boolean) : Float = {
    val sat = cond(name)
    if (direct) {
      if (sat) 0 else RMAX
    }else{
      if (sat) RMAX else 0
    }
  }

  private def parallel_of_resistances(nodes: Array[HNode], direct : Boolean): Float = {
    val sum = sum_resistances(nodes,direct)
    val prod = multipy_resistances(nodes,direct)

    if (sum == 0)
      0
    else if (sum==RINF)
      RINF
    else
      prod / sum
  }


  private def sum_resistances(nodes: Array[HNode], direct : Boolean): Float = {
    val head = nodes.head
    val tail = nodes.tail

    if (tail.length == 0)
      deduce_resistance(head,direct)
    else {
      val sum = sum_resistances(tail,direct) + deduce_resistance(head,direct)
      if (sum<RINF) sum else RINF
    }
  }

  private def multipy_resistances(nodes: Array[HNode], direct : Boolean): Float = {
    val head = nodes.head
    val tail = nodes.tail

    if (tail.length == 0)
      deduce_resistance(head,direct)
    else
      math.max(multipy_resistances(tail,direct) * deduce_resistance(head,direct), RINF)
  }



}
