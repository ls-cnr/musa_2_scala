package org.icar.musa.actor_model

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import org.icar.fol.Entail
import org.icar.ltl.supervisor.{NetSupervisor, SupervisorBuilder}
import org.icar.musa.context.EnvContext
import org.icar.musa.pmr._
import org.icar.musa.specification.{DomainLoader, EndToEnd, OnDemand}
import org.icar.musa.workflow.{WorkflowGrounding, WorkflowState}

/*
TODO manca un goal supervisor che dica che nello stato corrente il goal sia stato soddisfatto o violato
 */

class Orchestrator_Actor(domain : DomainLoader, sol : Solution) extends Actor with ActorLogging {

  case class GenerateConcreteWorkflow_Goal()
  case class SelectNextTask_Goal(current: WfItem)
  case class SelectScenario_Goal()

  var workflow_state : WorkflowState = new WorkflowState(sol)
  var workflow_grounding : WorkflowGrounding = new WorkflowGrounding()

  val supervisor_manager = new SupervisorBuilder
  var goal_supervisor : NetSupervisor = null

  var next_alternatives : List[WfTask] = List()
  var selected_task : WfTask = null

  var current_context = new EnvContext

  var grounder_actor : ActorRef = create_my_grounder

  override def preStart : Unit = {
    log.info("ready")

    domain.grounder_type match {
      case EndToEnd() =>
        log.debug("wait_end_to_end_grounding")
        context.become(wait_end_to_end_grounding)
        self ! GenerateConcreteWorkflow_Goal()

      case OnDemand() =>
        log.debug("orchestrate")
        context.become(orchestrate)
        self ! SelectNextTask_Goal(sol.start)
    }
  }

  override def postStop(): Unit = {
    log.info("completed")
  }

  override def receive: Receive = workflow_terminated


  /* state */
  def wait_end_to_end_grounding : Receive = {
    case GenerateConcreteWorkflow_Goal() =>
      grounder_actor ! SearchAllComplete(sol)

    case MappingAllConcrete(full_grounding: Map[String, ActorRef]) =>
      log.debug("all mappings")
      workflow_grounding.mapping = full_grounding
      transition_wait_end_to_end_grounding_TO_orchestrate(sol)

    case UncoveredConcretes =>
      log.info("some worker is missing")
    // TODO: RAISE "CHANGE WORKFLOW" EVENT

    case _ =>

  }

  /* transition */
  def transition_wait_end_to_end_grounding_TO_orchestrate(sol:Solution): Unit = {
    for (worker_actor <- workflow_grounding.mapping.values)
      worker_actor ! "join"
    log.debug("orchestrate")
    context.become(orchestrate.orElse(state_dispatcher))
    self ! SelectNextTask_Goal(sol.start)
  }

  /* state */
  def orchestrate : Receive = {
    case SelectNextTask_Goal(item) =>
      /*if (goal_supervisor!=null) {
        log.info("current state of goal: "+goal_supervisor.current_state)
        log.info("goal = "+domain.goal)
        log.info("w = "+current_context.state_of_world)
      }*/

      if (goal_supervisor==null || !goal_supervisor.isAccepted)
        next_alternatives = next(item)
      else
        next_alternatives = List()

      //log.info("now...")
      //for (i <- next_alternatives) log.info("Task: "+i.cap.name)

      if (!next_alternatives.isEmpty)
        transition_orchestrate_TO_waiting_next_task
      else {
        // WORKFLOW IS TERMINATED CORRECTLY
        transition_orchestrate_TO_workflow_terminated
      }
  }

  /* transition */
  def transition_orchestrate_TO_waiting_next_task : Unit = {
    log.debug("waiting_next_task")
    context.become(waiting_next_task)
  }

  /* transition */
  def transition_orchestrate_TO_workflow_terminated : Unit = {
    release_workers
    log.debug("workflow_terminated")
    context.become(workflow_terminated)
  }

  /* state */
  def waiting_next_task : Receive = {

    case ContextUpdate( env ) =>
      log.debug("context update")
      current_context = env

      // check all preconditions
      // the capability that matches will be executed
      val selected = check_alternatives()
      if (selected.isDefined) {
        selected_task = selected.get
        val map = workflow_grounding.mapping.get(selected_task.cap.name)
        if (map.isDefined)
          transition_waiting_next_task_TO_wait_post_conditions(map.get)
        else
          transition_waiting_next_task_TO_wait_for_worker(selected_task.cap.name)

      }

  }

  /* transition */
  def transition_waiting_next_task_TO_wait_post_conditions(worker : ActorRef) : Unit = {
    worker ! "go"
    log.debug("wait_post_conditions")
    context.become(wait_post_conditions.orElse(state_dispatcher))
  }


  /* transition */
  def transition_waiting_next_task_TO_wait_for_worker(cap_name : String) : Unit = {
    grounder_actor ! SearchConcrete(cap_name)
    log.debug("wait_for_worker")
    context.become(wait_for_worker)
  }


  /* state */
  def wait_for_worker : Receive = {
    case MappingConcrete(name, worker) =>
      log.debug("obtained actor for "+name+" ("+worker.path+")")
      workflow_grounding.mapping += (name -> worker)
      transition_wait_for_worker_TO_wait_post_conditions(worker)

    case UncoveredConcrete(name) =>
      log.info("no actor for "+name)
    // TODO: RAISE "CHANGE WORKFLOW" EVENT


  }

  /* transition */
  def transition_wait_for_worker_TO_wait_post_conditions(worker : ActorRef) : Unit = {
    worker ! "join"
    worker ! "go"
    log.debug("wait_post_conditions")
    context.become(wait_post_conditions.orElse(state_dispatcher))
  }


  /* state */
  def wait_post_conditions : Receive = {
    case TaskCompleted(abs_name,scn_name) =>
      transition_wait_post_conditions_TO_orchestrate(selected_task)

    //case _ =>

  }

  /* transition */
  def transition_wait_post_conditions_TO_orchestrate(task : WfTask): Unit = {
    log.debug("orchestrate")
    context.become(orchestrate.orElse(state_dispatcher))
    self ! SelectNextTask_Goal(task)
  }

  /* state */
  def workflow_terminated : Receive = {
    case _ =>

  }


  def state_dispatcher : Receive = {
    /* bottom-up */
    case SimulatedStateUpdate(evo_scn) =>
      context.parent ! SimulatedStateUpdate(evo_scn)

    case MeasurablesUpdate(m) =>
      context.parent ! MeasurablesUpdate(m)

    /* top-down */
    case ContextUpdate(environment_context) =>
      current_context = environment_context
      log.debug("state update")
      if (goal_supervisor==null)
        goal_supervisor = supervisor_manager.initialize(domain.goal.ltl,environment_context.state_of_world,domain.assumption)
      else
        goal_supervisor = supervisor_manager.update(goal_supervisor,environment_context.state_of_world,domain.assumption)
      if (goal_supervisor.isAccepted)
        log.debug("ACCEPTED STATE")
      for (worker <- workflow_grounding.mapping.values)
        worker ! ContextUpdate(environment_context)

    case _ =>

  }


  private def check_alternatives() : Option[WfTask] = {
    var selected : Option[WfTask] = None
    for (t <- next_alternatives if !selected.isDefined) {
      val pre = t.cap.pre
      val flag = Entail.condition(current_context.state_of_world,domain.assumption,pre)
      if (flag == true)
        selected = Some(t)
      //else
        //log.info("no alternative in "+current_context.state_of_world)
    }

    selected
  }

  def release_workers : Unit = {
    log.debug("release workers")
    for (map <- workflow_grounding.mapping) {
      map._2 ! "leave"
    }
  }


  private def next(item: WfItem): List[WfTask] = {
    var list : List[WfTask] = List()
    val outs = sol.arcs_out_from(item)
    for (out <- outs) {
      val nexts_following: List[WfTask] = follow(out)
      list = nexts_following ::: list
    }

    list
  }

  private def follow(flow : WfFlow) : List[WfTask] = {
    flow.to match {
      case t : WfTask => List(t)
      case g : WfGateway => next(g)
      case e : WfEndEvent => List()
    }
  }


  def create_my_grounder(): ActorRef = {
    val grounder_props = Props.create(classOf[Grounder_Actor],domain)
    context.actorOf(grounder_props, "grounder")
  }


}
