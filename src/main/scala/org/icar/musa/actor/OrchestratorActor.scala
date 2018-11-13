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

  var workflow_state : WorkflowState = new WorkflowState()
  var workflow_grounding : WorkflowGrounding = new WorkflowGrounding()

  init


  private def init : Unit = {
    log.info("ready")

    context.system.eventStream.subscribe(self,classOf[StateUpdate])
    context.system.eventStream.subscribe(self,classOf[SingleSolution])

  }

  /*
  def create_worker_actor(concreteClass: Class[_], abs_name : String, repository : Array[AbstractCapability] ) : Unit = {

    val abstract1 = recover_abstract(abs_name,repository)
    if (abstract1.isDefined) {
      val concrete1 = concreteClass.asSubclass(concreteClass)
      val worker_prop1 = Props.create(classOf[WorkerActor],concrete1, ps.ass_set)
      val context_actor : ActorRef = context.actorOf(worker_prop1, "wk_check_wakeup_1")
    }

  }*/


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

  def create_workers : Unit = {
    var repository = load_capabilities

    val abstract1 = recover_abstract("check_wake_up",repository)
    if (abstract1.isDefined) {
      val concrete1 = new CheckWakeUp1(abstract1.get)
      val worker_prop1 = Props.create(classOf[WorkerActor],concrete1, ps.ass_set)
      val worker_actor1 : ActorRef = context.actorOf(worker_prop1, "wk_check_wakeup_1")
      worker_actor1 ! "join"
    }

    val abstract2 = recover_abstract("remind_wake_up",repository)
    if (abstract2.isDefined) {
      val concrete2 = new RemindWakeUp1(abstract2.get)
      val worker_prop2 = Props.create(classOf[WorkerActor],concrete2, ps.ass_set)
      val worker_actor2 : ActorRef = context.actorOf(worker_prop2, "wk_remind_wake_up_1")
      worker_actor2 ! "join"
    }

    val abstract3 = recover_abstract("alert_anomaly",repository)
    if (abstract3.isDefined) {
      val concrete3 = new AlertAnomaly1(abstract3.get)
      val worker_prop3 = Props.create(classOf[WorkerActor],concrete3, ps.ass_set)
      val worker_actor3 : ActorRef = context.actorOf(worker_prop3, "wk_alert_anomaly_1")
      worker_actor3 ! "join"
    }

  }


}
