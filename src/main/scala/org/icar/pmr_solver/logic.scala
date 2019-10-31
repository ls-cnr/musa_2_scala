package org.icar.pmr_solver

trait predicate_formula
case class xx(index:Int) extends predicate_formula
case class tt() extends predicate_formula
case class ff() extends predicate_formula
case class conjunction(left:predicate_formula,right:predicate_formula) extends predicate_formula
case class disjunction(left:predicate_formula,right:predicate_formula) extends predicate_formula
case class negation(op:predicate_formula) extends predicate_formula
case class implies(left:predicate_formula,right:predicate_formula) extends predicate_formula
case class iff(left:predicate_formula,right:predicate_formula) extends predicate_formula




trait axiom
case class consequence(head:xx, cause:term_conjunction) extends axiom
case class term_conjunction(terms:Array[xx])


trait linear_temporal_formula
case class p(index:Int) extends predicate_formula
case class truth() extends predicate_formula
case class falsity() extends predicate_formula
case class and(left:linear_temporal_formula, right:linear_temporal_formula) extends predicate_formula
case class or(left:linear_temporal_formula, right:linear_temporal_formula) extends predicate_formula
case class neg(op:linear_temporal_formula) extends predicate_formula
case class implication(left:linear_temporal_formula, right:linear_temporal_formula) extends predicate_formula
case class doubleimplication(left:linear_temporal_formula, right:linear_temporal_formula) extends predicate_formula
case class next(op:linear_temporal_formula) extends predicate_formula
case class until(left:linear_temporal_formula, right:linear_temporal_formula) extends predicate_formula
case class release(left:linear_temporal_formula, right:linear_temporal_formula) extends predicate_formula
case class globally(op:linear_temporal_formula) extends predicate_formula
case class future(op:linear_temporal_formula) extends predicate_formula






