package org.icar.musa.pmr

import org.icar.musa.context.StateOfWorld
import org.icar.musa.spec.AbstractCapability

import scala.collection.mutable.ArrayBuffer

abstract class TerminationDescription {
  def get_score_threshold(): Option[Int] = None
}

case class AndTermination(termin: ArrayBuffer[TerminationDescription]) extends TerminationDescription {
  override def get_score_threshold(): Option[Int] = {
    var s: Option[Int] = None
    for (t <- termin)
      s = t.get_score_threshold()
    s
  }
}

case class OrTermination(termin: ArrayBuffer[TerminationDescription]) extends TerminationDescription {
  override def get_score_threshold(): Option[Int] = {
    var s: Option[Int] = None
    for (t <- termin)
      s = t.get_score_threshold()
    s
  }
}

case class FullGoalAchievementTermination(max_exit_nodes: Int) extends TerminationDescription

case class TimeTermination(millisec: Long) extends TerminationDescription

case class IterationTermination(its: Int) extends TerminationDescription
case class MaxEmptyIterationTermination(its: Int) extends TerminationDescription

case class ScoreTermination(score_threshold: Int, max_exitable_nodes: Int) extends TerminationDescription {
  override def get_score_threshold(): Option[Int] = Some(score_threshold)
}




class WTSLocalBuilder(ps: SingleGoalProblemSpecification, w: StateOfWorld, cap_set: Array[AbstractCapability], term: TerminationDescription) {
  private val explorer = new SingleGoalProblemExploration(ps, w, cap_set)
  val sol_builder = new SolutionBuilder(explorer.root)
  val wts: WTS = new WTS(explorer.root)

  private val score_th = term.get_score_threshold()

  private var start_time: Long = 0
  var num_exit_node = 0
  var num_scored_node = 0
  var num_empty_its = 0

  def build_wts(): Unit = {
    start_time = System.currentTimeMillis / 1000

    while (!check_termination(term)) {
      println("++++   it="+explorer.iteration+"  ++++++")
      sol_builder.log_state

      explorer.execute_iteration()
      val exp_opt = explorer.highest_expansion
      if (exp_opt.isDefined) {
        val exp = exp_opt.get
        //println("exp="+exp)
        wts.addExpansion(exp)

        exp match {
          case x: SimpleWTSExpansion =>
            explorer.pick_expansion(x)
            sol_builder.deal_with_expansion(x)
            if (!x.end.su.isAccepted)
              explorer.new_node(x.end)
            update_number_of_exit_and_score_nodes(x.end)


          case x : MultiWTSExpansion =>
            explorer.pick_expansion(x)
            sol_builder.deal_with_multi_expansion(x)
            for (e <- x.evo.values) {
              if (!e.su.isAccepted)
                explorer.new_node(e)

              update_number_of_exit_and_score_nodes(e)
            }


          case _ =>
        }

      } else {
        num_empty_its += 1
      }
    }
  }

  private def check_termination(t: TerminationDescription): Boolean = {
    term match {
      case x: AndTermination => check_termination_and(x.termin)
      case x: OrTermination => check_termination_or(x.termin)
      case x: FullGoalAchievementTermination => num_exit_node >= x.max_exit_nodes
      case x: TimeTermination => val c_time = System.currentTimeMillis / 1000; c_time >= x.millisec
      case x: IterationTermination => explorer.iteration >= x.its
      case x: ScoreTermination => num_scored_node >= x.max_exitable_nodes
      case x: MaxEmptyIterationTermination => num_empty_its >= x.its
      case _ => false
    }
  }

  private def check_termination_and(termin: ArrayBuffer[TerminationDescription]): Boolean = {
    var b = true
    var i = 0
    while (b) {
      if (!check_termination(termin(i))) b = false
      i += 1
    }
    b
  }

  private def check_termination_or(termin: ArrayBuffer[TerminationDescription]): Boolean = {
    var b = false
    var i = 0
    while (!b) {
      if (check_termination(termin(i))) b = true
      i += 1
    }

    b
  }

  private def update_number_of_exit_and_score_nodes (end: WTSStateNode): Unit = {
    if (end.su.isAccepted)
    num_exit_node += 1

    if (score_th.isDefined)
    if (end.su.distance_to_satisfaction < score_th.get)
    num_scored_node += 1
  }

}
