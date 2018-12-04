package org.icar.musa.actor_model

import akka.actor.{Actor, ActorLogging, ActorSystem}
import org.icar.musa.scenarios.sps.SPSSelectionStrategy
import org.icar.musa.spec.DomainLoader

class Validator_Actor (domain : DomainLoader) extends Actor with ActorLogging {
  case class CheckSelection()

  val strategy = new SPSSelectionStrategy

  override def preStart : Unit = {
    strategy.init

    val system = ActorSystem("MUSA")
    import system.dispatcher
    system.scheduler.scheduleOnce(strategy.check_delay , self, CheckSelection() )

    log.info("ready")
  }

  override def receive: Receive = {
    case Validate(sol) =>
      log.debug("validating new solution")
      /* validation... */
      strategy.update(sol)

    case CheckSelection() =>
      if (strategy.check_selection.isDefined)
        context.parent ! ValidatedAndSelected( strategy.check_selection.get )
      else {
        val system = ActorSystem("MUSA")
        import system.dispatcher
        system.scheduler.scheduleOnce(strategy.check_delay , self, CheckSelection() )
      }
  }

}
