package org.icar.musa.actor

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import org.icar.musa.context.StateOfWorld
import org.icar.musa.pmr.{SingleGoalProblemSpecification, Solution, WfTask}
import org.icar.musa.spec.{AllInOneWorkflow, DomainLoader, ManyAlternativeWorkflows}
import org.icar.musa.workflow.{WorkflowGrounding, WorkflowState}

class OrchestratorActor(self_conf_actor : ActorRef, domain : DomainLoader) extends Actor with ActorLogging {
  import context._

  var wi_opt : Option[StateOfWorld] = None
  var solution_opt : Option[Solution] = None

  var workflow_state : WorkflowState = null
  var workflow_grounding : WorkflowGrounding = new WorkflowGrounding()

  var grounder_actor : ActorRef = _
  var validator_actor : ActorRef = _

  override def preStart : Unit = {
    log.info("ready")

    context.system.eventStream.subscribe(self,classOf[StateUpdate])
    domain.solution_type match {
      case AllInOneWorkflow() =>
        context.system.eventStream.subscribe(self,classOf[SingleSolution])
        grounder_actor = context.actorOf(Props.create(classOf[GrounderActor],domain), "grounder")

      case ManyAlternativeWorkflows() =>
        context.system.eventStream.subscribe(self,classOf[MultiSolution])
        validator_actor = context.actorOf(Props.create(classOf[ValidatorActor],domain), "validator")
        grounder_actor = context.actorOf(Props.create(classOf[GrounderActor],domain), "grounder")
    }


  }

  override def receive: Receive = free

  def free : Receive = {
    case StateUpdate( w ) =>
      if (!wi_opt.isDefined) {
        log.debug("received_state")
        wi_opt = Some(w)
        self_conf_actor ! SelfConfigureRequest(w)
      }

    case SingleSolution( s ) =>
      log.debug("received_solution")
      log.debug(s.to_graphviz_string())

      workflow_state = new WorkflowState(s)
      become(orchestrating)
      self ! "orchestrate"

    case MultiSolution(set) =>
      log.debug("received_solutions")
      for (s <- set) {
        log.debug(s.to_graphviz_string())
        validator_actor ! Validate(s)
      }


    case _ =>
  }

  def orchestrating : Receive = {
    case "orchestrate" =>
      if (!workflow_state.current_tasks.isEmpty) {
        for (t <- workflow_state.current_tasks)
          if (workflow_grounding.mapping.contains(t.cap.name)) {
            val worker_actor = workflow_grounding.mapping.get(t.cap.name).get
            worker_actor ! "go"
          } else {
            log.debug("ask concrete for "+t.cap.name)
            grounder_actor ! SearchConcrete(t.cap.name)
          }

      } else {
        release_workers
        become(free)
      }

    case MappingConcrete(name, worker_actor) =>
      log.debug("obtained actor for "+name+" ("+worker_actor.path+")")
      workflow_grounding.mapping += (name -> worker_actor)
      worker_actor ! "join"
      worker_actor ! "go"

    case UncoveredConcrete(name) =>
      log.info("no actor for "+name)
      // TODO: RAISE "CHANGE WORKFLOW" EVENT


    case TaskCompleted(abs_name,scn_name) =>
      workflow_state.completed(abs_name)
      workflow_state.take_scenario(scn_name)
      self ! "orchestrate"
  }



  def release_workers : Unit = {
    log.info("Workflow terminated")
    for (a <- workflow_grounding.mapping) {
      a._2 ! "leave"
    }
  }


  /*
    override def receive: Receive = {

      case StateUpdate( w ) =>
        if (!wi_opt.isDefined) {
          log.debug("received_state")
          wi_opt = Some(w)
          self_conf_actor ! SelfConfigureRequest(w)
          log.debug("requested solutions")
        }


      case SingleSolution( s ) =>
        log.debug("received_solution")
        log.info(s.to_graphviz_string())
        //create_workers

        log.debug("starting orchestration")
        workflow_state = new WorkflowState(s)
        orchestrate


      case MappingConcrete(name, concrete) =>
        val worker_prop = Props.create(classOf[WorkerActor], concrete, domain.assumption)
        val worker_actor : ActorRef = context.actorOf(worker_prop, "wk_" + concrete.name)
        workflow_grounding.mapping += (concrete.abs_cap.name -> (concrete, worker_actor))
        worker_actor ! "join"
        worker_actor ! "go"


      case TaskCompleted(abs_name,scn_name) =>
        workflow_state.completed(abs_name)
        workflow_state.take_scenario(scn_name)
        orchestrate


    }

    def orchestrate : Unit = {
      if (workflow_state.current_tasks.isEmpty)
        release_workers

      else
        for (t <- workflow_state.current_tasks)
          activate_worker(t)

    }

    private def activate_worker(t: WfTask) = {
      if (workflow_grounding.mapping.contains(t.cap.name)) {
        val item = workflow_grounding.mapping.get(t.cap.name).get
        val act = item._2
        act ! "go"
      } else {
        grounder_actor ! SearchConcrete(t.cap.name)
      }
    }
  */

  /*
    def load_capabilities : Array[AbstractCapability] = {
      val sc = new PRINWakeUpScenario //PRINEntertainmentScenario
      sc.capabilities
    }

    def recover_abstract(str: String, repository: Array[AbstractCapability]): Option[GroundedAbstractCapability] = {
      var cap : Option[GroundedAbstractCapability] = None

      for (c <- repository if c.name==str)
        cap = Some(c.asInstanceOf[GroundedAbstractCapability])

      cap
    }

  def create_workers : Unit = {
    var repository = load_capabilities

    val abstract1 = recover_abstract("check_wake_up",repository)
    if (abstract1.isDefined) {
      val concrete1 = new CheckWakeUp1(abstract1.get)
      val worker_prop1 = Props.create(classOf[WorkerActor],concrete1, ps.ass_set)
      val worker_actor1 : ActorRef = context.actorOf(worker_prop1, "wk_check_wakeup_1")
      workflow_grounding.mapping += ("check_wake_up" -> (concrete1,worker_actor1))
      worker_actor1 ! "join"
    }

    val abstract2 = recover_abstract("remind_wake_up",repository)
    if (abstract2.isDefined) {
      val concrete2 = new RemindWakeUp1(abstract2.get)
      val worker_prop2 = Props.create(classOf[WorkerActor],concrete2, ps.ass_set)
      val worker_actor2 : ActorRef = context.actorOf(worker_prop2, "wk_remind_wake_up_1")
      workflow_grounding.mapping += ("remind_wake_up" -> (concrete2,worker_actor2))
      worker_actor2 ! "join"
    }

    val abstract3 = recover_abstract("alert_anomaly",repository)
    if (abstract3.isDefined) {
      val concrete3 = new AlertAnomaly1(abstract3.get)
      val worker_prop3 = Props.create(classOf[WorkerActor],concrete3, ps.ass_set)
      val worker_actor3 : ActorRef = context.actorOf(worker_prop3, "wk_alert_anomaly_1")
      workflow_grounding.mapping += ("alert_anomaly" -> (concrete3,worker_actor3))
      worker_actor3 ! "join"
    }

  }*/


}
