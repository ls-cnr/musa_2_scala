package org.icar.actor_model

import akka.actor.{Actor, ActorLogging}
import org.icar.pmr_solver.high_level_specification.{Domain, LTLGoalSet}
import org.icar.pmr_solver.symbolic_level.{HL2Raw_Map, RawGoalModelSupervisor, RawLTL, RawState}

class ModelCheckerActor(domain:Domain,goal_model:LTLGoalSet) extends Actor with ActorLogging {
	val raw_domain = new HL2Raw_Map(domain)
	val specifications: Array[RawLTL] = for (g<-goal_model.goals) yield raw_domain.ltl_formula(g)

	var some_supervisor : Option[RawGoalModelSupervisor] = None

	override def receive: Receive = {
		case WorldMsg.Update(state:RawState,_) =>
			if (!some_supervisor.isDefined)
				some_supervisor = Some( RawGoalModelSupervisor.factory(state,specifications))

			model_check(state)
	}

	private def model_check(state: RawState): Unit = ???


}
