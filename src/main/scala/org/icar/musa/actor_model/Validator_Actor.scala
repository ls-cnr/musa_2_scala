package org.icar.musa.actor_model

import akka.actor.{Actor, ActorLogging, ActorSystem}
import org.icar.musa.pmr.Solution
import org.icar.musa.specification._

import scala.concurrent.ExecutionContextExecutor

class Validator_Actor (domain : DomainLoader) extends Actor with ActorLogging {
  case class CheckSelection()
  case class Validate_Next_Solution_Goal()

  val system = ActorSystem("MUSA")
  implicit val executionContext: ExecutionContextExecutor = system.dispatcher

  lazy val selection_strategy : SelectionStrategy = domain.selection_strategy.getOrElse(new FirstInSelectionStrategy())
  val validation_strategy : ValidationStrategy = domain.validation_strategy.getOrElse(new AcceptingAllStrategy())

  var solutions_to_validate : List[Solution] = List()

  override def preStart : Unit = {
    selection_strategy.init

    system.scheduler.scheduleOnce(selection_strategy.check_delay , self, CheckSelection() )
    system.scheduler.scheduleOnce(selection_strategy.check_delay , self, Validate_Next_Solution_Goal() )

    log.info("ready")
  }

  override def receive: Receive = {
    case Validate(sol) =>
      log.debug("new solution to validate")
      //sol.print_for_graphviz()
      solutions_to_validate = sol :: solutions_to_validate

    case StopValidation() =>
      solutions_to_validate = List()

    case Validate_Next_Solution_Goal() =>
      if (solutions_to_validate.nonEmpty) {
        log.debug("validating new solution")

        val sol = solutions_to_validate.head
        solutions_to_validate = solutions_to_validate.tail

        /* validation... */
        val result_of_validation = validation_strategy.validate(sol)

        if (result_of_validation==true)
          selection_strategy.update(sol)
      }
      system.scheduler.scheduleOnce(selection_strategy.check_delay , self, Validate_Next_Solution_Goal() )

    case CheckSelection() =>
      if (selection_strategy.check_selection.isDefined) {
        context.parent ! ValidatedAndSelected( selection_strategy.check_selection.get )
        self ! StopValidation()
      } else {
        system.scheduler.scheduleOnce(selection_strategy.check_delay , self, CheckSelection() )
      }
  }

}
