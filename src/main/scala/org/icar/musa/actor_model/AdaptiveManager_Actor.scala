package org.icar.musa.actor_model

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import org.icar.musa.context.{EnvContext, StateOfWorld}
import org.icar.musa.pmr.Solution
import org.icar.musa.specification.{DomainLoader, EarlyDecisionWorkflow, LateDecisionWorkflows}

class AdaptiveManager_Actor (domain : DomainLoader, env:EnvContext) extends Actor with ActorLogging {

  case class SearchInitialState_Goal()
  case class ConfigureSolution_Goal(wi: StateOfWorld)
  case class RunSolution_Goal(s: Solution)

  /* sub-actors are created dynamically */
  var context_actor : ActorRef = _
  var self_conf_actor : ActorRef = _
  var validator_actor : ActorRef = _
  var orchestrator_actor : ActorRef = _

  override def preStart : Unit = {
    log.info("ready")
    self ! SearchInitialState_Goal()
  }

  override def postStop(): Unit = {
    log.info("completed")
  }

  override def receive: Receive = waiting_initial_state

  def waiting_initial_state : Receive = {
    case SearchInitialState_Goal() =>
      context_actor = create_my_context(env)

    case ContextUpdate( env ) =>
      log.debug("received_state")
      context.become(waiting_configuration)
      self ! ConfigureSolution_Goal(env.state_of_world)

    case _ =>
  }

  def waiting_configuration : Receive = {
    case ConfigureSolution_Goal(wi) =>
      self_conf_actor = create_my_self_configurator(wi)

      domain.solution_type match {
        case EarlyDecisionWorkflow() =>
          context.become(waiting_single_solution)

        case LateDecisionWorkflows() =>
          validator_actor = create_my_validator()
          context.become(waiting_multi_solutions)
      }

  }

  def waiting_single_solution : Receive = {
    case SingleSolution( s ) =>
      log.debug("received_solution ")
      log.debug(s.to_graphviz_string())
      context.become(adaptive_orchestration.orElse(state_dispatcher))
      self ! RunSolution_Goal(s)

    case "configuraion error" =>

    case _ =>
  }

  def waiting_multi_solutions : Receive = {
    case MultiSolution(set) =>
      log.debug("received_solutions")
      for (s <- set) {

        //val world = domain.quality_asset.pretty_string(s.final_state_of_world.get)
        //val qos = domain.quality_asset.evaluate_state(s.final_state_of_world.get)
        //log.info("received_solution "+qos+" <-- "+world)

        log.debug(s.to_graphviz_string())
        validator_actor ! Validate(s)
      }

    case "configuration error" =>

    case ValidatedAndSelected(s) =>
      log.debug("selected solution")
      context.become(adaptive_orchestration.orElse(state_dispatcher))
      self ! RunSolution_Goal(s)

    case _ =>
  }

  def adaptive_orchestration : Receive = {
    case RunSolution_Goal(s) =>

      self_conf_actor ! TerminateSelfConfiguration()

      orchestrator_actor = create_my_orchestrator(s)

    case "workflow terminated" =>

    case "grounding error" =>

    case "task error" =>
  }


  def waiting_termination : Receive = {
    case "" =>
  }



  def state_dispatcher : Receive = {
    /* bottom-bottom */
    case ContextUpdate(environment_context) =>
      log.debug("inform_state")
      orchestrator_actor ! ContextUpdate(environment_context)

    case SimulatedStateUpdate(evo_scn) =>
      context_actor ! SimulatedStateUpdate(evo_scn)

    case MeasurablesUpdate(m) =>
      context_actor ! MeasurablesUpdate(m)


    case _ =>

  }


  def create_my_context(env: EnvContext): ActorRef = {
    val context_props = Props.create(classOf[Context_Actor],domain,env)
    context.actorOf(context_props, "context")
  }
  def create_my_self_configurator(wi: StateOfWorld): ActorRef = {
    val context_props = Props.create(classOf[SelfConf_Actor],domain,wi)
    context.actorOf(context_props, "self-conf")
  }
  def create_my_validator(): ActorRef = {
    val context_props = Props.create(classOf[Validator_Actor],domain)
    context.actorOf(context_props, "validator")
  }
  def create_my_orchestrator(s : Solution): ActorRef = {
    val context_props = Props.create(classOf[Orchestrator_Actor],domain,s)
    context.actorOf(context_props, "orchestrator")
  }

}
