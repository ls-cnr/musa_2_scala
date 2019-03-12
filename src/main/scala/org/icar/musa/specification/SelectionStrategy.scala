package org.icar.musa.specification

import org.icar.musa.pmr.Solution

import scala.concurrent.duration.{FiniteDuration, _}

abstract class SelectionStrategy {

  def init() : Unit
  def update(sol : Solution) : Unit
  def check_selection : Option[Solution]
  def terminate() : Unit
  def check_delay : FiniteDuration

}


class FirstInSelectionStrategy extends SelectionStrategy {
  var selected : Option[Solution] = None

  override def init: Unit = {}

  override def update(sol: Solution): Unit = {
    if (!selected.isDefined)
      selected = Some(sol)
  }

  override def check_selection: Option[Solution] = selected

  override def terminate: Unit = {}

  def check_delay : FiniteDuration = 10 seconds
}