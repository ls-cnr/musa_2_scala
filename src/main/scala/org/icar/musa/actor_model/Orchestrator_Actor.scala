package org.icar.musa.actor_model

import akka.actor.{Actor, ActorLogging, ActorRef, PoisonPill, Props}
import org.icar.musa.context.{EnvContext, StateOfWorld}
import org.icar.musa.pmr.Solution
import org.icar.musa.specification._
import org.icar.musa.workflow.{WorkflowGrounding, WorkflowState}

class Orchestrator_Actor(domain : DomainLoader, env:EnvContext) extends Actor with ActorLogging {
  case class Orchestrate_Goal()

  var workflow_grounding : WorkflowGrounding = new WorkflowGrounding()
  var workflow_state : WorkflowState = _

  /* sub-actors are created dynamically */
  var context_actor : ActorRef = _
  var grounder_actor : ActorRef = _
  var validator_actor : ActorRef = _
  var self_conf_actor : ActorRef = _


  override def preStart : Unit = {
    log.info("ready")
    context_actor = create_my_context(env)
    context.become(waiting_initial_state)
  }

  override def postStop(): Unit = {
    log.info("completed")
  }


  def waiting_initial_state : Receive = {
    case ContextUpdate( env ) =>
      val w = env.state_of_world
      log.debug("received_state")
      self_conf_actor = create_my_self_configurator(w)

      domain.solution_type match {
        case EarlyDecisionWorkflow() =>
          context.become(waiting_single_solution)

        case LateDecisionWorkflows() =>
          validator_actor = create_my_validator()
          context.become(waiting_multi_solutions)
      }

    case _ =>
  }

  def waiting_single_solution : Receive = {
    case SingleSolution( s ) =>
      log.debug("received_solution")
      log.debug(s.to_graphviz_string())
      translate_solution_into_workflow(s)

    case _ =>
  }

  def waiting_multi_solutions : Receive = {
    case MultiSolution(set) =>
      log.info("received_solutions")
      for (s <- set) {
        log.debug(s.to_graphviz_string())
        validator_actor ! Validate(s)
      }

    case ValidatedAndSelected(s) =>
      log.debug("selected solution")
      self_conf_actor ! TerminateSelfConfiguration()
      translate_solution_into_workflow(s)

    case _ =>
  }

  private def translate_solution_into_workflow(s: Solution) = {
    grounder_actor = create_my_grounder()
    workflow_state = new WorkflowState(s)
    domain.grounder_type match {
      case EndToEnd() =>
        grounder_actor ! SearchAllComplete(s)
        context.become(waiting_workflow)

      case OnDemand() =>
        context.become(executing)
        self ! Orchestrate_Goal()

    }
  }


  def waiting_workflow : Receive = {
    case MappingAllConcrete(worker_grounding: Map[String, ActorRef]) =>
      log.debug("all mappings")
      workflow_grounding.mapping = worker_grounding
      for (worker_actor <- workflow_grounding.mapping.values)
        worker_actor ! "join"
      context.become(executing)
      self ! Orchestrate_Goal()

    case UncoveredConcretes =>
      log.info("some actor is missing")
    // TODO: RAISE "CHANGE WORKFLOW" EVENT

    case _ =>
  }

  def executing : Receive = {
    case Orchestrate_Goal() =>
      orchestrate_step

    case TaskCompleted(abs_name,scn_name) =>
      workflow_state.completed(abs_name)
      workflow_state.take_scenario(scn_name)
      self ! Orchestrate_Goal()

    case MappingConcrete(name, worker_actor) =>
      log.debug("obtained actor for "+name+" ("+worker_actor.path+")")
      workflow_grounding.mapping += (name -> worker_actor)
      worker_actor ! "join"
      worker_actor ! "go"

    case UncoveredConcrete(name) =>
      log.info("no actor for "+name)
    // TODO: RAISE "CHANGE WORKFLOW" EVENT


    /* forwarding */
    case SimulatedStateUpdate(evo_scn) =>
      context_actor ! SimulatedStateUpdate(evo_scn)

    case MeasurablesUpdate(m) =>
      context_actor ! MeasurablesUpdate(m)

    case ContextUpdate(environment_context) =>
      for (a <- workflow_grounding.mapping) {
        a._2 ! ContextUpdate(environment_context)
      }

  }

  def orchestrate_step = {
    if (!workflow_state.current_tasks.isEmpty) {
      for (t <- workflow_state.current_tasks)
        if (workflow_grounding.mapping.contains(t.cap.name)) {
          val worker_actor = workflow_grounding.mapping.get(t.cap.name).get
          worker_actor ! "go"
        } else {
          log.debug("ask concrete for " + t.cap.name)
          grounder_actor ! SearchConcrete(t.cap.name)
        }

    } else {
      release_workers
      context.become(workflow_terminated)
    }
  }

  def check_mapping_completeness: Boolean = ???

  def release_workers : Unit = {
    log.info("Workflow terminated")
    for (a <- workflow_grounding.mapping) {
      a._2 ! "leave"
    }

    /*
    if (context_actor != null)
      context stop context_actor
    if (grounder_actor != null)
      context stop grounder_actor
    if (validator_actor != null)
      context stop validator_actor
    if (self_conf_actor != null)
      context stop self_conf_actor*/
    context stop self
  }

  def activate_all_workers : Unit = {
    log.info("Workflow initiated")
    for (a <- workflow_grounding.mapping) {
      a._2 ! "join"
    }
  }

  def workflow_terminated : Receive = {
    case _ =>
  }

  override def receive: Receive = {
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
  def create_my_grounder(): ActorRef = {
    val grounder_props = Props.create(classOf[Grounder_Actor],domain)
    context.actorOf(grounder_props, "grounder")
  }





}
