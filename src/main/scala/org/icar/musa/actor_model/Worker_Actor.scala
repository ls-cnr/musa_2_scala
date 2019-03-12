package org.icar.musa.actor_model

import akka.actor.{Actor, ActorLogging, ActorRef}
import org.icar.fol.{AssumptionSet, Entail}
import org.icar.musa.context.{Measurables, StateOfWorld}
import org.icar.musa.main_entity.ConcreteCapability

class Worker_Actor (concrete_cap : ConcreteCapability,ass_set: AssumptionSet,recruiter:ActorRef) extends Actor with ActorLogging {

  override def preStart : Unit = {
    log.info("ready")

    context.become(free)
  }

  def free : Receive = {
    case "join" =>
      log.debug("joined")
      concrete_cap.init
      context.become(ready)
  }

  def ready : Receive = {
    case "go" =>
      log.debug("ready for "+concrete_cap.name)
      concrete_cap.pre_start
      context.become(waiting_pre_conditions)

    case "leave" =>
      concrete_cap.terminate
      context.become(free)

  }

  def waiting_pre_conditions : Receive = {
    case ContextUpdate( env ) =>
      log.debug("state update")
      check_pre_conditions_and_datain(env.state_of_world,env.measurables)

    case "leave" =>
      concrete_cap.compensate
      context.become(free)

  }


  def waiting_post_conditions : Receive = {
    case ContextUpdate( env ) =>
      val w = env.state_of_world
      check_post_conditions(w)

    case "leave" =>
      concrete_cap.compensate
      context.become(free)

  }

  override def receive: Receive = free

  private def check_pre_conditions_and_datain(w: StateOfWorld, m : Measurables): Unit = {
    log.debug("checking pre-condition and data for " + concrete_cap.name)
    if (Entail.condition(w, ass_set, concrete_cap.abs_cap.pre)) {
      log.debug("conditions true")
      val in_opt : Option[Measurables] = m.getData(concrete_cap.abs_cap.in)
      if (in_opt.isDefined) {
        log.debug("data ready")
        concrete_cap.execute(w,in_opt.get)
        log.debug("executed " + concrete_cap.name + ": " + concrete_cap.scn)

        val evo_opt = concrete_cap.get_simulated_scenario
        val out = concrete_cap.output

        if (evo_opt.isDefined)
          recruiter ! SimulatedStateUpdate(evo_opt.get)

        if (out.size>0)
          recruiter !  MeasurablesUpdate(out)

        context.become(waiting_post_conditions)
      } else
        log.info("data not ready")


    } else
      log.info("conditions false")

  }

  private def check_post_conditions(w: StateOfWorld): Unit = {
    log.info("checking post-conditions for " + concrete_cap.name + " that is " + concrete_cap.abs_cap.post)
    if (Entail.condition(w, ass_set, concrete_cap.abs_cap.post)) {
      concrete_cap.post_end

      log.info("task completed for " + recruiter.path)
      recruiter ! TaskCompleted(concrete_cap.abs_cap.name, concrete_cap.scn)

      context.become(ready)
    }
  }

}
