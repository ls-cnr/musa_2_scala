package org.icar.ltl.supervisor

import org.icar.fol.AssumptionSet
import org.icar.ltl._
import org.icar.musa.context.StateOfWorld
import org.icar.petrinet.AnnotatedTransition
//import petrinet.logic.Petrinet

import scala.collection.mutable.ArrayBuffer

case class NetHierarchy private(root : HNode)

class NetHierarchyBuilder {
  var suff = 0

  def build(f : ltlFormula) : NetHierarchy = {
    suff=0
    NetHierarchy( node_from_formula(f) )
  }

   private def node_from_formula(formula : ltlFormula) : HNode = {
    formula match {
      case LogicAtom(predicate)           => ConditionNode("p"+suffix,LogicAtom(predicate))
      case Globally(sub)                  => GloballyNode("G"+suffix, node_from_formula(sub))
      case Finally(sub)                   => FinallyNode("F"+suffix, node_from_formula(sub))
      case Next(sub)                      => NextNode("X"+suffix, node_from_formula(sub))
      case Until(sub1,sub2)               => UntilNode("U"+suffix, node_from_formula(sub1),node_from_formula(sub2))
      case Release(sub1,sub2)             => ReleaseNode("R"+suffix, node_from_formula(sub1),node_from_formula(sub2))
      case LogicImplication(sub1,sub2)    => ImplyNode("->"+suffix,node_from_formula(sub1),node_from_formula(sub2))
      case LogicBiImplication(sub1,sub2)  => BidImplyNode("<->"+suffix,node_from_formula(sub1),node_from_formula(sub2))
      case LogicConjunction(subs) =>
        val arr : ArrayBuffer[HNode] = for (a <- subs) yield node_from_formula(a)
        AndNode("&"+suffix,arr.toArray)
      case LogicDisjunction(subs) =>
        val arr : ArrayBuffer[HNode] = for (a <- subs) yield node_from_formula(a)
        OrNode("|"+suffix,arr.toArray)
      case LogicNegation(sub) => NotNode("!"+suffix, node_from_formula(sub))
      case _ => null
    }
  }

  private def suffix : Int = { suff=suff+1; suff }
}


