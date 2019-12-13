package org.icar.actor_model

import akka.actor.{ActorRef, Props}
import org.icar.actor_model.protocol.{AdaptationProtocol, ContextProtocol, GoalProtocol}
import org.icar.pmr_solver.high_level_specification.{GroundPredicate, HL_GroundLiteral, HL_LTLFormula, NegatedGroundPredicate}
import org.icar.pmr_solver.rete.{RETE, RETEBuilder}
import org.icar.pmr_solver.symbolic_level.{HL2Raw_Map, RawGoalModelSupervisor, RawLTL, RawState}

import scala.org.icar.symbolic_level.R2S

abstract class ContextMng(config:ApplicationConfig) extends MUSAActor {

	val raw_domain = new HL2Raw_Map(config.domain)
	val belief_base = init_belief_base
	var hl_goals:Array[HL_LTLFormula] = Array()
	var raw_goals:Array[RawLTL] = Array()
	var runtime_checker : RawGoalModelSupervisor = init_supervisor

	var monitor_list : Array[ActorRef] = init_monitors(config.monitors)


	override def receive: Receive = {

		case msg:GoalProtocol.InformGoalListChanged =>
			hl_goals = msg.goals
			raw_goals = for (g<-hl_goals) yield raw_domain.ltl_formula(g)
			react_to_goal_change

		case ContextProtocol.InformObservations(_,preds) =>
			react_to_observation(preds)

		case ContextProtocol.InformInternalUpdate(_,log) =>
			react_to_internal(log.index)

		case _ =>
	}

	private def react_to_goal_change : Unit = {
		runtime_checker = init_supervisor

		// check violated goals
		val violated_goals = check_violated_goals
		if (violated_goals.nonEmpty)
			context.parent ! AdaptationProtocol.goal_violation(violated_goals)

	}

	private def check_violated_goals : Array[HL_LTLFormula] = {
		var violated_goals:List[HL_LTLFormula] = List.empty
		for (index <- 0 until hl_goals.size) {
			val su = runtime_checker.sups(index)
			if (!su.success)
				violated_goals = hl_goals(index) :: violated_goals
		}
		violated_goals.toArray
	}

	private def react_to_state_change: Unit = {
		val state = belief_base.state
		if (hl_goals.nonEmpty) {
			runtime_checker = runtime_checker.getNext(state)

			// check goals are violated
			val violated_goals = check_violated_goals
			if (violated_goals.nonEmpty){
				context.parent ! AdaptationProtocol.goal_violation(violated_goals)
			} else {
				val r2s = R2S.calculate_goals_resistance(state,raw_goals)
				context.parent ! ContextProtocol.context_update(state,r2s)
			}
		}
	}

	private def react_to_observation(preds:List[HL_GroundLiteral]):Unit = {
		var changes = false
		for (p <- preds) {
			p match {
				case pred:GroundPredicate =>
					val index = raw_domain.direct(pred)
					if (belief_base.state.bit_descr(index) == false) {
						changes = true
						belief_base.add_fact(index)
					}

				case neg:NegatedGroundPredicate =>
					val index = raw_domain.direct(neg.p)
					if (belief_base.state.bit_descr(index) == true) {
						changes = true
						belief_base.retract_fact(index)
					}

				case _ =>
			}
		}

		if (changes)
			react_to_state_change
	}

	private def react_to_internal(index:Int) = {
		var changes = false
		if (belief_base.state.bit_descr(index)==false) {
			changes=true
			belief_base.add_fact(index)
		}
		if (changes)
			react_to_state_change
	}

	private def init_monitors(monitors:Array[EnvObserver]): Array[ActorRef] =
		for (m<-monitors) yield init_monitor_actor(m)

	private def init_monitor_actor(env_monitor : EnvObserver) : ActorRef = {
		val props = MonitorMng.instance(config,env_monitor)
		context.actorOf(props,env_monitor.variable_description+"_mon")
	}
	private def init_belief_base: RETE = {
		val wi: Array[Boolean] = raw_domain.state_of_world(config.background_state.statements)
		val rete = RETEBuilder.factory(config.axioms,raw_domain,RawState(wi))
		rete.execute
		rete
	}
	private def init_supervisor : RawGoalModelSupervisor = {
		val init_supervisor = RawGoalModelSupervisor.factory(belief_base.state,raw_goals)
		init_supervisor
	}


}


object ContextMng {
	def instance(config:ApplicationConfig) : Props = Props.create(classOf[ContextMng],config)
}
