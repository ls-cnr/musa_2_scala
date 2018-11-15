package org.icar.musa.actor

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import org.icar.musa.actor.concrete.{AlertAnomaly1, CheckWakeUp1, RemindWakeUp1}
import org.icar.musa.context.StateOfWorld
import org.icar.musa.pmr.{SingleGoalProblemSpecification, Solution}
import org.icar.musa.scenarios.PRINWakeUpScenario
import org.icar.musa.spec.{AbstractCapability, GroundedAbstractCapability}
import org.icar.musa.workflow.{WorkflowGrounding, WorkflowState}

class OrchestratorActor(ps : SingleGoalProblemSpecification, self_conf_actor:ActorRef, musa_db : DBInfo, domain_id : Int) extends Actor with ActorLogging {
  var wi_opt : Option[StateOfWorld] = None
  var solution_opt : Option[Solution] = None

  var workflow_state : WorkflowState = null
  var workflow_grounding : WorkflowGrounding = new WorkflowGrounding()

  init


  private def init : Unit = {
    log.info("ready")

    context.system.eventStream.subscribe(self,classOf[StateUpdate])
    context.system.eventStream.subscribe(self,classOf[SingleSolution])

  }


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
      create_workers

      log.debug("starting orchestration")
      workflow_state = new WorkflowState(s)
      orchestrate

    case Completed(abs_name,scn_name) =>
      workflow_state.completed(abs_name)
      workflow_state.take_scenario(scn_name)
      orchestrate


  }

  def orchestrate : Unit = {
    if (workflow_state.current_tasks.isEmpty)
      release_workers

    else
      for (t <- workflow_state.current_tasks) {
        val item_opt = workflow_grounding.mapping.get(t.cap.name)
        if (item_opt.isDefined){
          val item=item_opt.get
          //val conc = item._1
          val act = item._2
          act ! "go"
        }
      }
  }


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

  def release_workers : Unit = {
    log.info("Workflow terminated")
    for (a <- workflow_grounding.mapping) {
      val act = a._2._2
      act ! "leave"
    }
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

  }


}
