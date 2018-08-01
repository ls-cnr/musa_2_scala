package org.icar.ltl.supervisor

import org.icar.fol.{AssumptionSet, Entail, GroundPredicate}
import org.icar.ltl.ltlFormula
import org.icar.musa.context.StateOfWorld
import org.icar.petrinet._

class SupervisorBuilder() {

  val entail = Entail

  def initialize(f: ltlFormula, w: StateOfWorld, ass: AssumptionSet): NetSupervisor = {
    val builder = new NetHierarchyBuilder
    val net = builder.build(f)
    val conditions = check_conditions(net.root,w,ass)
    val petrinets = init_petrinet_map(net.root)
    val updatedpetrinets = update_petrinet_map(net.root,petrinets,conditions)
    val current = deduce_state(net.root, updatedpetrinets, conditions)
    val partial_satisfaction = new ResistanceToFullAchievement(net.root, w, conditions, x => petrinet_state(x,updatedpetrinets) )
    new NetSupervisor(net, updatedpetrinets,current,partial_satisfaction.r)
  }

  def update(sup : NetSupervisor, w: StateOfWorld, ass: AssumptionSet): NetSupervisor = {
    //val start_update_timestamp: Long = System.currentTimeMillis
    val conditions = check_conditions(sup.net.root,w,ass)
    //val after_condition_timestamp: Long = System.currentTimeMillis
    val petrinets = update_petrinet_map(sup.net.root,sup.petrinets,conditions)
    //val after_petrinets_timestamp: Long = System.currentTimeMillis
    val current = deduce_state(sup.net.root, petrinets, conditions)
    //val after_deduce_timestamp: Long = System.currentTimeMillis
    val partial_satisfaction = new ResistanceToFullAchievement(sup.net.root, w, conditions, x => petrinet_state(x,petrinets) )
    //val after_partial_sat_timestamp: Long = System.currentTimeMillis

    //println("check conditions ms: "+(after_condition_timestamp-start_update_timestamp))
    //println("check petrinets ms: "+(after_petrinets_timestamp-after_condition_timestamp))
    //println("deduce state ms: "+(after_deduce_timestamp-after_petrinets_timestamp))
    //println("check partial sat ms: "+(after_partial_sat_timestamp-after_deduce_timestamp))


    new NetSupervisor(sup.net,petrinets,current,partial_satisfaction.r)
  }

  private def check_conditions(root: HNode, w: StateOfWorld, ass: AssumptionSet) : Map[String, Boolean] = {
    val cond_map = conditions_map(root)
    entail.condition_map(w,ass,cond_map)
  }

  private def update_petrinet_map(node: HNode, petrinets : Map[String, Petrinet], conditions : Map[String, Boolean]): Map[String, Petrinet] = {
    node match {
      case ConditionNode(_,_) => Map[String, Petrinet]()
      case NotNode(_,subnode) => update_petrinet_map(subnode,petrinets,conditions)
      case AndNode(_,subnodes) => update_petrinet_map_with_subnodes(subnodes,petrinets,conditions)
      case OrNode(_,subnodes) => update_petrinet_map_with_subnodes(subnodes,petrinets,conditions)
      case ImplyNode(_,sub1,sub2) => update_petrinet_map_with_subnodes(Array(sub1,sub2),petrinets,conditions)
      case BidImplyNode(_,sub1,sub2) => update_petrinet_map_with_subnodes(Array(sub1,sub2),petrinets,conditions)

      case GloballyNode(_,subnode) => updatePNNode(node.name,Array(subnode),petrinets,conditions)
      case FinallyNode(_,subnode) => updatePNNode(node.name,Array(subnode),petrinets,conditions)
      case NextNode(_,subnode) => updatePNNode(node.name,Array(subnode),petrinets,conditions)
      case UntilNode(_,node1,node2) => updatePNNode(node.name,Array(node1,node2)++node.asInstanceOf[UntilNode].subnodes,petrinets,conditions)
      case ReleaseNode(_,node1,node2) => updatePNNode(node.name,Array(node1,node2),petrinets,conditions)

      case _ => Map[String, Petrinet]()
    }
  }

  private def update_petrinet_map_with_subnodes(subnodes: Array[HNode], petrinets: Map[String, Petrinet], conditions: Map[String, Boolean]): Map[String, Petrinet] = {
    var pns = Map[String, Petrinet]()
    for (n <- subnodes)
      pns = pns ++ update_petrinet_map(n,petrinets,conditions)

    pns
  }

  private def updatePNNode(name: String, nodes: Array[HNode], petrinets: Map[String, Petrinet], conditions: Map[String, Boolean]): Map[String, Petrinet] = {
    val pns = update_petrinet_map_with_subnodes(nodes, petrinets, conditions)

    val dy = petrinets(name).dynamic.clone()
    val pn = Petrinet(petrinets(name).structure,dy)
    val fireable = pn.get_fireable_transitions

    var fired=false

    for (t <- fireable) {
      val fire = t match {
        case t_direct: AnnotatedDirectTransition =>
          val node = get_subnode(nodes, t_direct.dependency)
          decide_if_fire(node, petrinets, conditions)
        case t_inverse: AnnotatedInverseTransition =>
          val node = get_subnode(nodes, t_inverse.dependency)
          !decide_if_fire(node, petrinets, conditions)
        case _: AnnotatedTruthTransition =>
          true
        case _ =>
          false
      }

      if (fire & !fired) {
        pn.fire(t)
        fired=true
      }
    }

    pns ++ Map(name -> pn)
  }

  def decide_if_fire(node: HNode,petrinets: Map[String, Petrinet], conditions: Map[String, Boolean]) : Boolean = {
    var b = false

    val state = deduce_state(node,petrinets,conditions)
    if (state == AcceptedState())
      b=true

    b
  }


  private def get_subnode(nodes: Array[HNode], dependency: String) : HNode = {
    var h : HNode = null

    for (n <- nodes)
      if (n.name.equals(dependency))
        h = n

    h
  }

  private def init_petrinet_map(node: HNode): Map[String, Petrinet] = {
    node match {
      case ConditionNode(_, _) => Map[String, Petrinet]()
      case AndNode(_, subnodes) => prepare_petrinet_map_from_subnodes(subnodes)
      case OrNode(_, subnodes) => prepare_petrinet_map_from_subnodes(subnodes)
      case NotNode(_, subnode) => init_petrinet_map(subnode)
      case ImplyNode(_, subnode1, subnode2) => init_petrinet_map(subnode1) ++ init_petrinet_map(subnode2)
      case BidImplyNode(_, subnode1, subnode2) => init_petrinet_map(subnode1) ++ init_petrinet_map(subnode2)
      case GloballyNode(name, subnode) => init_petrinet_map(subnode) ++ Map(name -> node.asInstanceOf[GloballyNode].initial_petrinet())
      case FinallyNode(name, subnode) => init_petrinet_map(subnode) ++ Map(name -> node.asInstanceOf[FinallyNode].initial_petrinet())
      case NextNode(name, subnode) => init_petrinet_map(subnode) ++ Map(name -> node.asInstanceOf[NextNode].initial_petrinet())
      case UntilNode(name, subnode1, subnode2) => init_petrinet_map(subnode1) ++ init_petrinet_map(subnode2) ++ Map(name -> node.asInstanceOf[UntilNode].initial_petrinet())
      case ReleaseNode(name, subnode1, subnode2) => init_petrinet_map(subnode1) ++ init_petrinet_map(subnode2) ++ Map(name -> node.asInstanceOf[ReleaseNode].initial_petrinet())
      case _ => Map[String, Petrinet]()
    }
  }
  private def prepare_petrinet_map_from_subnodes(subnodes: Array[HNode]): Map[String, Petrinet] = {
    if (subnodes.length == 1)
      init_petrinet_map(subnodes(0))
    else
      init_petrinet_map(subnodes.head) ++ prepare_petrinet_map_from_subnodes(subnodes.tail)
  }

  private def conditions_map(node: HNode): Map[String,GroundPredicate] = {
    node match {
      case ConditionNode(name, atom) => Map(name -> atom.predicate)
      case AndNode(_, subnodes) => conditions_map_from_subnodes(subnodes)
      case OrNode(_, subnodes) => conditions_map_from_subnodes(subnodes)
      case NotNode(_, subnode) => conditions_map(subnode)
      case ImplyNode(_, subnode1, subnode2) => conditions_map(subnode1) ++ conditions_map(subnode2)
      case BidImplyNode(_, subnode1, subnode2) => conditions_map(subnode1) ++ conditions_map(subnode2)
      case GloballyNode(_, subnode) => conditions_map(subnode)
      case FinallyNode(_, subnode) => conditions_map(subnode)
      case NextNode(_, subnode) => conditions_map(subnode)
      case UntilNode(_, subnode1, subnode2) => conditions_map(subnode1) ++ conditions_map(subnode2)
      case ReleaseNode(_, subnode1, subnode2) => conditions_map(subnode1) ++ conditions_map(subnode2)
      case _ => Map[String, GroundPredicate]()
    }
  }

  private def conditions_map_from_subnodes(subnodes: Array[HNode]): Map[String, GroundPredicate] = {
    if (subnodes.length == 1)
      conditions_map(subnodes(0))
    else
      conditions_map(subnodes.head) ++ conditions_map_from_subnodes(subnodes.tail)
  }

  private def deduce_state(node: HNode,petrinets: Map[String, Petrinet], conditions: Map[String, Boolean]): PlaceType = {
    node match {
      case ConditionNode(name, _) => if ( conditions(name) ) AcceptedState() else ErrorState()
      case AndNode(_, subnodes) => and_truth_table(subnodes,petrinets,conditions)
      case OrNode(_, subnodes) => or_truth_table(subnodes,petrinets,conditions)
      case NotNode(_, subnode) => not_truth_table(subnode,petrinets,conditions)
      case ImplyNode(_, subnode1, subnode2) => imply_truth_table(subnode1, subnode2,petrinets,conditions)
      case BidImplyNode(_, subnode1, subnode2) => bid_imply_truth_table(subnode1, subnode2,petrinets,conditions)
      case GloballyNode(name, _) => petrinet_state(name,petrinets)
      case FinallyNode(name, _) => petrinet_state(name,petrinets)
      case NextNode(name, _) => petrinet_state(name,petrinets)
      case UntilNode(name, _, _) => petrinet_state(name,petrinets)
      case ReleaseNode(name, _, _) => petrinet_state(name,petrinets)
      case _ => ErrorState()
    }
  }

  def petrinet_state(name : String,petrinets: Map[String, Petrinet]) : PlaceType = {
    val pn = petrinets(name)
    var pess_reply : PlaceType = ErrorState()
    val places_with_tokens = pn.get_places_with_tokens
    for (p <- places_with_tokens)
      pess_reply = p.state

    pess_reply
  }

  private def and_truth_table(nodes: Array[HNode],petrinets: Map[String, Petrinet], conditions: Map[String, Boolean]): PlaceType = truth_table(nodes,and_definition,petrinets,conditions)
  private def or_truth_table(nodes: Array[HNode],petrinets: Map[String, Petrinet], conditions: Map[String, Boolean]): PlaceType = truth_table(nodes,or_definition,petrinets,conditions)
  private def not_truth_table(node: HNode,petrinets: Map[String, Petrinet], conditions: Map[String, Boolean]): PlaceType = not_definition(deduce_state(node,petrinets,conditions))
  private def imply_truth_table(node1: HNode, node2: HNode,petrinets: Map[String, Petrinet], conditions: Map[String, Boolean]): PlaceType = imply_definition(deduce_state(node1,petrinets,conditions),deduce_state(node2,petrinets,conditions))
  private def bid_imply_truth_table(node1: HNode, node2: HNode,petrinets: Map[String, Petrinet], conditions: Map[String, Boolean]): PlaceType = bid_imply_definition(deduce_state(node1,petrinets,conditions),deduce_state(node2,petrinets,conditions))

  private def truth_table(nodes: Array[HNode], op: (PlaceType,PlaceType) => PlaceType,petrinets: Map[String, Petrinet], conditions: Map[String, Boolean] ): PlaceType = {
    val head = nodes.head
    val tail = nodes.tail

    if (tail.length == 1)
      op(deduce_state(head,petrinets,conditions),deduce_state(tail(0),petrinets,conditions))
    else
      op( deduce_state(head,petrinets,conditions), truth_table(tail,op,petrinets,conditions) )
  }


  private def and_definition(a: PlaceType, b: PlaceType): PlaceType = {
    (a, b) match {
      case (AcceptedState(), AcceptedState()) => AcceptedState()
      case (AcceptedState(), WaitAcceptedState()) => AcceptedState()
      case (AcceptedState(), WaitErrorState()) => ErrorState()
      case (AcceptedState(), ErrorState()) => ErrorState()
      case (WaitAcceptedState(), AcceptedState()) => AcceptedState()
      case (WaitAcceptedState(), WaitAcceptedState()) => WaitAcceptedState()
      case (WaitAcceptedState(), WaitErrorState()) => WaitErrorState()
      case (WaitAcceptedState(), ErrorState()) => ErrorState()
      case (WaitErrorState(), AcceptedState()) => WaitErrorState()
      case (WaitErrorState(), WaitAcceptedState()) => WaitErrorState()
      case (WaitErrorState(), WaitErrorState()) => WaitErrorState()
      case (WaitErrorState(), ErrorState()) => ErrorState()
      case (ErrorState(), AcceptedState()) => ErrorState()
      case (ErrorState(), WaitAcceptedState()) => ErrorState()
      case (ErrorState(), WaitErrorState()) => ErrorState()
      case _ => ErrorState()
    }
  }
  private def or_definition(a: PlaceType, b: PlaceType): PlaceType = {
    (a, b) match {
      case (AcceptedState(), AcceptedState()) => AcceptedState()
      case (AcceptedState(), WaitAcceptedState()) => AcceptedState()
      case (AcceptedState(), WaitErrorState()) => AcceptedState()
      case (AcceptedState(), ErrorState()) => AcceptedState()
      case (WaitAcceptedState(), AcceptedState()) => AcceptedState()
      case (WaitAcceptedState(), WaitAcceptedState()) => WaitAcceptedState()
      case (WaitAcceptedState(), WaitErrorState()) => WaitAcceptedState()
      case (WaitAcceptedState(), ErrorState()) => WaitAcceptedState()
      case (WaitErrorState(), AcceptedState()) => AcceptedState()
      case (WaitErrorState(), WaitAcceptedState()) => WaitAcceptedState()
      case (WaitErrorState(), WaitErrorState()) => WaitErrorState()
      case (WaitErrorState(), ErrorState()) => WaitErrorState()
      case (ErrorState(), AcceptedState()) => AcceptedState()
      case (ErrorState(), WaitAcceptedState()) => WaitAcceptedState()
      case (ErrorState(), WaitErrorState()) => WaitErrorState()
      case _ => ErrorState()
    }
  }
  private def not_definition(a: PlaceType): PlaceType = {
    a match {
      case AcceptedState() => ErrorState()
      case WaitAcceptedState() => WaitErrorState()
      case WaitErrorState() => WaitAcceptedState()
      case ErrorState() => AcceptedState()
      case _ => ErrorState()
    }
  }
  private def imply_definition(a: PlaceType, b: PlaceType): PlaceType = {
    (a, b) match {
      case (AcceptedState(), AcceptedState()) => AcceptedState()
      case (AcceptedState(), WaitAcceptedState()) => WaitAcceptedState()
      case (AcceptedState(), WaitErrorState()) => WaitErrorState()
      case (AcceptedState(), ErrorState()) => ErrorState()
      case (WaitAcceptedState(), AcceptedState()) => AcceptedState()
      case (WaitAcceptedState(), WaitAcceptedState()) => WaitAcceptedState()
      case (WaitAcceptedState(), WaitErrorState()) => WaitErrorState()
      case (WaitAcceptedState(), ErrorState()) => ErrorState()
      case (WaitErrorState(), AcceptedState()) => AcceptedState()
      case (WaitErrorState(), WaitAcceptedState()) => AcceptedState()
      case (WaitErrorState(), WaitErrorState()) => AcceptedState()
      case (WaitErrorState(), ErrorState()) => AcceptedState()
      case (ErrorState(), AcceptedState()) => AcceptedState()
      case (ErrorState(), WaitAcceptedState()) => AcceptedState()
      case (ErrorState(), WaitErrorState()) => AcceptedState()
      case _ => AcceptedState()
    }
  }
  private def bid_imply_definition(a: PlaceType, b: PlaceType): PlaceType = {
    (a, b) match {
      case (AcceptedState(), AcceptedState()) => AcceptedState()
      case (AcceptedState(), WaitAcceptedState()) => WaitAcceptedState()
      case (AcceptedState(), WaitErrorState()) => WaitErrorState()
      case (AcceptedState(), ErrorState()) => ErrorState()
      case (WaitAcceptedState(), AcceptedState()) => WaitAcceptedState()
      case (WaitAcceptedState(), WaitAcceptedState()) => WaitAcceptedState()
      case (WaitAcceptedState(), WaitErrorState()) => WaitErrorState()
      case (WaitAcceptedState(), ErrorState()) => AcceptedState()
      case (WaitErrorState(), AcceptedState()) => WaitErrorState()
      case (WaitErrorState(), WaitAcceptedState()) => WaitErrorState()
      case (WaitErrorState(), WaitErrorState()) => AcceptedState()
      case (WaitErrorState(), ErrorState()) => AcceptedState()
      case (ErrorState(), AcceptedState()) => ErrorState()
      case (ErrorState(), WaitAcceptedState()) => AcceptedState()
      case (ErrorState(), WaitErrorState()) => AcceptedState()
      case _ => AcceptedState()
    }
  }


}
