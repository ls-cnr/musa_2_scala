package org.icar.musa.actor_model

import akka.actor.{Actor, ActorLogging, ActorSystem}
import org.icar.musa.specification._

class Validator_Actor (domain : DomainLoader) extends Actor with ActorLogging {
  case class CheckSelection()

  val system = ActorSystem("MUSA")
  implicit val executionContext = system.dispatcher

  val selection_strategy : SelectionStrategy = domain.selection_strategy.getOrElse(new FirstInSelectionStrategy())
  val validation_strategy : ValidationStrategy = domain.validation_strategy.getOrElse(new AcceptingAllStrategy())

  override def preStart : Unit = {
    selection_strategy.init

    system.scheduler.scheduleOnce(selection_strategy.check_delay , self, CheckSelection() )

    log.info("ready")
  }

  override def receive: Receive = {
    case Validate(sol) =>
      log.debug("validating new solution")

      /* validation... */
      val result_of_validation = validation_strategy.validate(sol)

      if (result_of_validation==true)
        selection_strategy.update(sol)


    case CheckSelection() =>
      if (selection_strategy.check_selection.isDefined)
        context.parent ! ValidatedAndSelected( selection_strategy.check_selection.get )
      else {
        system.scheduler.scheduleOnce(selection_strategy.check_delay , self, CheckSelection() )
      }
  }

}
