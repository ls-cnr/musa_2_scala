package org.icar.musa.actor

import akka.actor.{Actor, ActorLogging, ActorSystem}
import javax.swing._
import org.icar.musa.pmr.Solution
import org.icar.musa.scenarios.sps.SPSSelectionStrategy
import org.icar.musa.spec.{DomainLoader, SelectionStrategy}



class ValidatorActor(domain : DomainLoader) extends Actor with ActorLogging {
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
    case Validate(x) =>
      log.info("validating new solution")
      /* validation... */
      strategy.update(x)

    case CheckSelection() =>
      if (strategy.check_selection.isDefined)
        context.parent ! SingleSolution( strategy.check_selection.get )
      else {
        val system = ActorSystem("MUSA")
        import system.dispatcher
        system.scheduler.scheduleOnce(strategy.check_delay , self, CheckSelection() )
      }
  }


}

