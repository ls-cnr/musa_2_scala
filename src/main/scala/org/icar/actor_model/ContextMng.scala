package org.icar.actor_model

import akka.actor.{ActorRef, Props}
import org.icar.actor_model.core.{ApplicationConfig, EnvObserver, MUSAActor, MUSALogger}
import org.icar.actor_model.protocol.{AdaptationProtocol, ContextProtocol, GoalProtocol}
import org.icar.actor_model.role.{ContextProducerRole, GoalChangeConsumerRole, InternalUpdateConsumerRole, ObservationConsumerRole}
import org.icar.pmr_solver.high_level_specification.{GroundPredicate, HL_LTLFormula, NegatedGroundPredicate}
import org.icar.pmr_solver.rete.{RETE, RETEBuilder}
import org.icar.pmr_solver.symbolic_level.{HL2Raw_Map, RawConj, RawDisj, RawFF, RawFinally, RawGlobally, RawGoalModelSupervisor, RawIff, RawImpl, RawLTL, RawNeg, RawNext, RawRelease, RawState, RawTT, RawUntil, RawVar}

import scala.org.icar.symbolic_level.R2S

class ContextMng(config:ApplicationConfig) extends MUSAActor
	with GoalChangeConsumerRole
	with ContextProducerRole
	with ObservationConsumerRole
	with InternalUpdateConsumerRole {

	val my_log_area = config.logfactory.register_actor(self.path.name)
	mylog("welcome to the ContextMng !")

	val raw_domain = new HL2Raw_Map(config.domain)
	val belief_base = init_belief_base
	var hl_goals:Array[HL_LTLFormula] = Array()
	var raw_goals:Array[RawLTL] = Array()
	var runtime_checker : RawGoalModelSupervisor = init_supervisor

	var monitor_list : Array[ActorRef] = init_monitors(config.monitors)

	override def receive: Receive = roles reduce {_ orElse _}

	override def preStart(): Unit = {
		context.parent ! register_to_internal_producer
		context.parent ! role__register_to_goal_changes
		for (mon <- monitor_list)
			mon ! register_to_observer
	}

	override def role__received_observation_update(sender: ActorRef, msg: ContextProtocol.InformObservations): Unit = {
		val preds = msg.preds

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
			react_to_some_change

	}

	override def role__received_internal_update(sender: ActorRef, msg: ContextProtocol.InformInternalUpdate): Unit = {
		val index = msg.log.index

		var changes = false
		if (belief_base.state.bit_descr(index)==false) {
			changes=true
			belief_base.add_fact(index)
		}
		if (changes)
			react_to_some_change
	}

	override def received_goals_update(sender: ActorRef, msg: GoalProtocol.InformGoalListChanged): Unit = {
		hl_goals = msg.goals
		raw_goals = for (g<-hl_goals) yield raw_domain.ltl_formula(g)
		runtime_checker = init_supervisor

		react_to_some_change
/*
		// check violated goals
		val violated_goals = check_violated_goals
		if (violated_goals.nonEmpty)
			notify_subscribers_of_goals_violation(violated_goals)
*/
	}

	private def check_violated_goals : List[HL_LTLFormula] = {
		var violated_goals:List[HL_LTLFormula] = List.empty
		for (index <- 0 until hl_goals.size) {
			val su = runtime_checker.sups(index)
			if (!su.success)
				violated_goals = hl_goals(index) :: violated_goals
		}
		violated_goals
	}

	private def react_to_some_change: Unit = {
		val state = belief_base.state
		if (hl_goals.nonEmpty) {
			runtime_checker = runtime_checker.getNext(state)

			// check goals are violated
			val violated_goals = check_violated_goals
			if (violated_goals.nonEmpty){
				notify_subscribers_of_goals_violation(violated_goals.toArray)
			} else {
				val r2s = R2S.calculate_goals_resistance(state,raw_goals)
				//val r2s : Float = 0
				notify_subscribers_of_context_updates(state,r2s)
			}
		}
	}


	/* Init Children */
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
		val sup = RawGoalModelSupervisor.factory(belief_base.state,raw_goals)
		sup
	}

}


object ContextMng {
	def instance(config:ApplicationConfig) : Props = Props.create(classOf[ContextMng],config)
}
