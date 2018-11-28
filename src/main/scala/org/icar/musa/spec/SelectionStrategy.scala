package org.icar.musa.spec

import org.icar.musa.pmr.Solution

import scala.concurrent.duration.FiniteDuration

abstract class SelectionStrategy {

  def init : Unit
  def update(sol : Solution) : Unit
  def check_selection : Option[Solution]
  def terminate : Unit
  def check_delay : FiniteDuration

}
