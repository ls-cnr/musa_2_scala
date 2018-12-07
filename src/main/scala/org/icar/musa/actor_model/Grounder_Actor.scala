package org.icar.musa.actor_model

import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem}
import org.icar.musa.pmr.WfTask
import org.icar.musa.specification.DomainLoader

import scala.concurrent.duration._

class Grounder_Actor (domain : DomainLoader) extends Actor with ActorLogging {
  case class CollectWorkerGoal(abs_name : String, requestor : ActorRef)
  case class ContactAllProviders(tasks: Set[WfTask])
  case class ContactAllProviders_Goal(tasks: Set[WfTask])
  case class CollectAllWorker_Goal(tasks: Set[WfTask])

  var providers_for_abstract : Map[String,ActorRef] = Map()
  var worker_grounding : Map[String,ActorRef] = Map()
  val collect_delay = 10 milliseconds


  override def preStart : Unit = {
    log.info("ready")
  }

  override def receive : Receive = waiting

  def waiting : Receive = {
    case SearchConcrete(abs_name : String) =>
      if (worker_grounding.contains(abs_name))
        context.parent ! MappingConcrete(abs_name, worker_grounding.get(abs_name).get )

      else {
        context.system.eventStream.publish( CallForProviders(abs_name,self) )
        context.become(task_grounding)

        import system.dispatcher
        val system : ActorSystem = ActorSystem("MUSA")
        system.scheduler.scheduleOnce(collect_delay, self, CollectWorkerGoal(abs_name,sender))
      }

    case SearchAllComplete(s) =>
      context.become(end_to_end)
      self ! ContactAllProviders(s.tasks)

    case _ =>
  }

  def task_grounding : Receive = {

    case ProviderResponse(abs,new_provider) =>
      if (!providers_for_abstract.contains(abs))
        providers_for_abstract += (abs -> new_provider)
      else {
        val prev_provider: ActorRef = providers_for_abstract(abs)
        val best_provider = qos_selection(prev_provider,new_provider)

        if (best_provider==new_provider) {
          providers_for_abstract = providers_for_abstract map {
            case (abs , prev_provider) => abs -> new_provider
            case x => x
          }
        }

      }

    case CollectWorkerGoal(abs_name,requestor) =>
      if (providers_for_abstract.contains(abs_name)) {
        val provider = providers_for_abstract(abs_name)
        provider ! SelectedForAbstractCapability(abs_name,context.parent)
      } else {
        context.parent ! UncoveredConcrete(abs_name)
        context.become(waiting)
      }


    case WorkerInstance(abs,worker_actor) =>
      worker_grounding += (abs -> worker_actor)
      context.parent ! MappingConcrete(abs, worker_actor )
      context.become(waiting)
  }


  def end_to_end : Receive = {
    case ContactAllProviders(tasks) =>
      for (t <- tasks)
        context.system.eventStream.publish( CallForProviders(t.cap.name,self) )

      import system.dispatcher
      val system : ActorSystem = ActorSystem("MUSA")
      system.scheduler.scheduleOnce(collect_delay, self, ContactAllProviders_Goal(tasks))


    case ProviderResponse(abs,new_provider) =>
      log.debug("provider "+new_provider.path+" for "+abs)
      if (!providers_for_abstract.contains(abs))
        providers_for_abstract += (abs -> new_provider)
      else {
        val prev_provider: ActorRef = providers_for_abstract(abs)
        val best_provider = qos_selection(prev_provider,new_provider)

        if (best_provider==new_provider) {
          providers_for_abstract = providers_for_abstract map {
            case (abs , prev_provider) => abs -> new_provider
            case x => x
          }
        }
      }

    case ContactAllProviders_Goal(tasks) =>
      var complete = true
      for (t <- tasks)
        if (!providers_for_abstract.contains(t.cap.name))
          complete = false

      if (complete==true) {
        for (t <- tasks) {
          val provider = providers_for_abstract(t.cap.name)
          provider ! SelectedForAbstractCapability(t.cap.name, context.parent)
        }
        import system.dispatcher
        val system : ActorSystem = ActorSystem("MUSA")
        system.scheduler.scheduleOnce(collect_delay, self, CollectAllWorker_Goal(tasks))
      } else {
        context.parent ! UncoveredConcretes()
        context.become(waiting)
      }

    case WorkerInstance(abs,worker_actor) =>
      worker_grounding += (abs -> worker_actor)

    case CollectAllWorker_Goal(tasks) =>
      context.parent ! MappingAllConcrete(worker_grounding)
      context.become(waiting)
  }

  def qos_selection(prev: ActorRef, provider: ActorRef) : ActorRef = provider



}


