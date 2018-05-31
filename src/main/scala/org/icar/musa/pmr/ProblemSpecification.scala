package org.icar.musa.pmr

import org.icar.fol.AssumptionSet
import org.icar.musa.context.StateOfWorld
import org.icar.musa.spec.LTLGoal

import scala.collection.mutable.ArrayBuffer

case class SingleGoalProblemSpecification(ass_set: AssumptionSet, goal : LTLGoal, asset : QualityAsset)
case class ProblemSpecification(ass_set: AssumptionSet, req_set : RequirementSet, asset : QualityAsset)

case class RequirementSet (goals : ArrayBuffer[LTLGoal], priority : Map[String,Int])

trait QualityAsset {
  def evaluate_node( w:StateOfWorld, goal_sat : Float ) : Float = goal_sat
  def evaluate_state( w:StateOfWorld ) : Option[Float] = None
  def max_score : Float = 0
  def pretty_string( w:StateOfWorld ) : String = w.toString
  def pretty_string(node: WTSStateNode): String
  def pretty_string(exp: WTSExpansion): String

}

class EmptyQualityAsset(val ass: AssumptionSet) extends QualityAsset {
  override def pretty_string(node: WTSStateNode): String = "node("+pretty_string(node.w)+","+node.su+")"
  override def pretty_string(exp: WTSExpansion): String = {
    exp match {
      case x : SimpleWTSExpansion =>
        val start = pretty_string(x.start.w)
        val end = pretty_string(x.end.w)
        "{("+start+" => "+end+" with "+x.cap.name+" ["+x.end.su.current_state+" dist="+x.end.su.distance_to_satisfaction+" ("+x.end.su.petrinets+")}"
      case x : MultiWTSExpansion =>
        val start = pretty_string(x.start.w)
        var end ="{"
        for (e <- x.evo)
          end += e._2+":"+pretty_string(e._2.w)+"["+e._2.su.current_state+"("+e._2.su.distance_to_satisfaction+")]},"

        "{("+start+" => "+end+" with "+x.cap.name+"}"
      case _ => ""
    }
  }

}


