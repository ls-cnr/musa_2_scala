package org.icar.musa.specification

import org.icar.fol.FOLCondition
import org.icar.musa.main_entity.LTLGoal

import scala.collection.mutable.ArrayBuffer

case class Mission(pre:FOLCondition, goals : ArrayBuffer[LTLGoal], priority : Map[String,Int])

