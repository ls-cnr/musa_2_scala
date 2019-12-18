package org.icar.actor_model.core

import org.icar.pmr_solver.high_level_specification.LTLGoalSet

import scala.org.icar.high_level_specification.Solution
import scala.org.icar.pmr_solver.best_first_planner.TerminationDescription

trait MessageContent
case class EmptyContent() extends MessageContent

trait Protocol {
	var id_counter:Long=0
	def get_id : Long = { val id=id_counter; id_counter += 1; id}
}
trait ProtocolPart  {
	val id : Long
}




case class NotUnderstood()


object AbstractSolutionMsg {
	case class EmptySet()
	case class Set(sol:Array[Solution])
	case class SetUpdate(sol:Array[Solution])
	case class UnvalidatedSet(sol:Array[Solution])
	case class ValidatedSet(validator_id:String,sol:Array[Solution])
	case class Selected(sol:Solution)
	case class SwitchTo(sol:Solution)
}

object TaskMsg {
	case class Progress(task_is:String,progress:Float)
	case class Completed(task_is:String)
	case class Failed(task_is:String)
}

object MeansEndRequestMsg {
	case class Full(term:TerminationDescription)
	case class FullOrPartial(term:TerminationDescription)
}


object AdaptationMsg {
	case class ServiceFailure()
	case class GoalViolation(violation:List[LTLGoalSet])
	case class NoApplicableSolution()
}

