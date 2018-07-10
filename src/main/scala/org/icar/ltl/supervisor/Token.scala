package org.icar.ltl.supervisor

import org.icar.petrinet.AnnotatedPlace
//import petrinet.logic.{Petrinet, Place}

/*
case class Token(place_name:String, net_name: String)


case class TokenConf(tokens : Token*)

object TokenConf {
  def from(node : HNode) : TokenConf = {
    node match {
      case ConditionNode(_,_) => TokenConf()
      case NotNode(_,subnode) => from(subnode)
      case AndNode(_,sub1,sub2) => fromMultiSubNodes(Array(sub1,sub2))
      case OrNode(_,sub1,sub2) => fromMultiSubNodes(Array(sub1,sub2))
      case ImplyNode(_,sub1,sub2) => fromMultiSubNodes(Array(sub1,sub2))
      case BidImplyNode(_,sub1,sub2) => fromMultiSubNodes(Array(sub1,sub2))

      case GloballyNode(_,subnode) => fromPNNode(node.asInstanceOf[GloballyNode],Array(subnode))
      case FinallyNode(_,subnode) => fromPNNode(node.asInstanceOf[FinallyNode],Array(subnode))
      case NextNode(_,subnode) => fromPNNode(node.asInstanceOf[NextNode],Array(subnode))
      case UntilNode(_,node1,node2) => fromPNNode(node.asInstanceOf[UntilNode],Array(node1,node2))
      case ReleaseNode(_,node1,node2) => fromPNNode(node.asInstanceOf[ReleaseNode],Array(node1,node2))

      case _ => TokenConf()
    }
  }

  private def fromMultiSubNodes(subnodes: Seq[HNode]): TokenConf = {
    var token_list : List[Token] = List()

    for (n <- subnodes)
      token_list = token_list ::: from(n).tokens.toList

    TokenConf(token_list:_*)
  }

  private def fromPNNode( node: PNNode, subnodes: Seq[HNode]) : TokenConf = {
    val mt = petri_net_start_tokens(node.petrinet())
    val all_tokens = Token(mt._1,mt._2) :: fromMultiSubNodes(subnodes).tokens.toList

    TokenConf(all_tokens:_*)
  }

  private def petri_net_start_tokens(pn : Petrinet) : (String, String) = {
    var place_name = ""
    val places : java.util.List[Place] = pn.getPlaces()
    for (i <- 0 until places.size()) {
      val p : AnnotatedPlace = places.get(i).asInstanceOf[AnnotatedPlace]
      if (p.initial)
        place_name = p.name

    }
    (place_name,pn.getName)
  }

}
*/
