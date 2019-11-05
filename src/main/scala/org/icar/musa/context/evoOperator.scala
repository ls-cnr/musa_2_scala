package org.icar.musa.context

import org.icar.fol._


sealed abstract class EvoOperator

case class Deprec_AddEvoOperator(add : GroundPredicate) extends EvoOperator

case class Deprec_RemoveEvoOperator(rmv : GroundPredicate) extends EvoOperator

case class Deprec_RemoveAllEvoOperator(rmv_all : String) extends EvoOperator

case class AddOperator(p : Predicate) extends EvoOperator
case class RmvOperator(p : Predicate) extends EvoOperator
