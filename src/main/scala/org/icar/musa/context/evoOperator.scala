package org.icar.musa.context

import org.icar.fol._

sealed abstract class EvoOperator

case class AddEvoOperator(add : GroundPredicate) extends EvoOperator

case class RemoveEvoOperator(rmv : GroundPredicate) extends EvoOperator

case class RemoveAllEvoOperator(rmv_all : String) extends EvoOperator
