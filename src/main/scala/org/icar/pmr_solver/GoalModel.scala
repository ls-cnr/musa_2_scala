package org.icar.pmr_solver

import org.icar.ltl.ltlFormula



abstract class GoalModel {
  def getSupervisor : GoalSupervisor
}
abstract class GoalSupervisor


class GoalSet(goals : Array[ltlFormula]) extends GoalModel {
  override def getSupervisor: GoalSupervisor = new GoalSetSupervisor
}

class GoalSetSupervisor extends GoalSupervisor







