package org.icar.musa.specification

import org.icar.musa.pmr.Solution


abstract class ValidationStrategy {

  def validate (sol:Solution) : Boolean

}



class AcceptingAllStrategy() extends ValidationStrategy {
  override def validate(sol: Solution): Boolean = true
}

class RefusingAllStrategy() extends ValidationStrategy {
  override def validate(sol: Solution): Boolean = false
}