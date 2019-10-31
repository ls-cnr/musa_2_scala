package org.icar.pmr_solver

case class SolverConfiguration (
	                              termination : TerminationDescription,
	                              sol_conf : SolutionConfiguration
                              )

/******* SOLVER TERMINATION CONDITION ********/
abstract class TerminationDescription
case class TimeTermination(millisec: Long) extends TerminationDescription
case class IterationTermination(its: Int) extends TerminationDescription
case class AndTermination(left:TerminationDescription,right:TerminationDescription) extends TerminationDescription
case class OrTermination(left:TerminationDescription,right:TerminationDescription) extends TerminationDescription



/******* SOLUTION VALIDITY CONDITION ********/
case class SolutionConfiguration(
	                                allow_loop : Boolean,
	                                allow_self_loop : Boolean,
	                                allow_cap_multiple_instance : Boolean,
	                                allow_parallel_action : Boolean
                                )


