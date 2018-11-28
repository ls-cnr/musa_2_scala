package org.icar.musa.actor

import akka.actor.{Actor, ActorLogging, ActorRef, ActorSystem}
import org.icar.musa.spec.DomainLoader

import scala.concurrent.duration._

class GrounderActor(domain : DomainLoader) extends Actor with ActorLogging {
  case class CollectWorkerGoal(abs_name : String, requestor : ActorRef)

  var provider_grounding : Map[String,ActorRef] = Map()
  var worker_grounding : Map[String,ActorRef] = Map()
  val collect_delay = 100 milliseconds

  override def preStart : Unit = {
    log.info("ready")
  }



  override def receive: Receive = {

    case SearchConcrete(abs_name : String) =>
      if (worker_grounding.contains(abs_name))
        context.parent ! MappingConcrete(abs_name, worker_grounding.get(abs_name).get )

      else {
        //log.info("call for providers: "+abs_name)
        context.system.eventStream.publish( CanProviderDoAbstractCapability(abs_name,self) )

        import system.dispatcher
        val system : ActorSystem = ActorSystem("MUSA")
        system.scheduler.scheduleOnce(collect_delay, self, CollectWorkerGoal(abs_name,sender))
      }



    case ProviderResponse(abs,new_provider) =>
      //log.info("reply from provider: "+new_provider.path)
      if (!provider_grounding.contains(abs))
        provider_grounding += (abs -> new_provider)
      else {
        val prev_provider: ActorRef = provider_grounding(abs)
        val best_provider = qos_selection(prev_provider,new_provider)

        if (best_provider==new_provider) {
          provider_grounding = provider_grounding map {
            case (abs , prev_provider) => abs -> new_provider
            case x => x
          }
        }

      }

    case CollectWorkerGoal(abs_name,requestor) =>
      //log.info("selecting provider and waiting worker")
      if (provider_grounding.contains(abs_name)) {
        val provider = provider_grounding(abs_name)
        provider ! ProviderHasBeenSelectedForAbstractCapability(abs_name,context.parent)
      } else
        requestor ! UncoveredConcrete(abs_name)



    case WorkerInstanceForEmployer(abs,worker_actor) =>
      //log.info("selected worker: "+worker_actor.path)
      worker_grounding += (abs -> worker_actor)
      context.parent ! MappingConcrete(abs, worker_actor )

  }


  def qos_selection(prev: ActorRef, provider: ActorRef) : ActorRef = provider


}



/* OLD
class GrounderActor(domain : DomainLoader) extends Actor with ActorLogging {
  val concrete_repository : Array[ConcreteCapability] = load_concrete_capabilities

  var workflow_grounding : Map[String,ConcreteCapability] = Map()

  init

  private def init : Unit = {
    log.info("ready")
  }

  override def receive: Receive = {

    case SearchConcrete(abs_name : String) =>
      if (workflow_grounding.contains(abs_name))
        sender() ! MappingConcrete(abs_name, workflow_grounding.get(abs_name).get )

      else {
        val selected = for (c <- concrete_repository if c.abs_cap.name == abs_name) yield c
        if (selected.length>0) {
          workflow_grounding += (abs_name -> selected(0))
          sender() ! MappingConcrete(abs_name,selected(0))
        }

      }

  }


  def load_concrete_capabilities: Array[ConcreteCapability] = {
    //val wakeup = new WakeUpConcreteRepository

    //wakeup.repository
    domain.concrete_repository
  }


}

 */
