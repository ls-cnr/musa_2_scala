package org.icar.actor_model.role

import akka.actor.ActorRef
import org.icar.actor_model.core.{MUSARole, MetaSolInfo}
import org.icar.actor_model.protocol.AbstractSolProtocol
import org.icar.actor_model.protocol.AbstractSolProtocol.{InformEmptySet, InformSolutions, RequestSolutions, RequestToValidatePlans}
import org.icar.pmr_solver.high_level_specification.LTLGoalSet
import org.icar.pmr_solver.symbolic_level.RawState

import scala.collection.mutable
import scala.org.icar.high_level_specification.Solution

trait SolutionCustomerRole extends MUSARole {
  def msg_to_request_solutions_for_problem(initial_state:RawState, goal_set:LTLGoalSet) = AbstractSolProtocol.init(initial_state,goal_set)

  def role__received_abstract_solutions(sender: ActorRef, msg: InformSolutions): Unit
  def role__received_empty_solutions(sender: ActorRef, msg: InformEmptySet): Unit

  registerRole {
    case msg:InformSolutions =>
      mylog("abstract solutions are ready")
      role__received_abstract_solutions(sender,msg)
    case msg:InformEmptySet =>
      mylog("no available abstract solutions")
      role__received_empty_solutions(sender,msg)
  }

}

trait SolutionProducerRole extends MUSARole {
  var planning_request : Option[(ActorRef,RequestSolutions)]=None

  def delegate_validation(sol:Array[Solution]) = {
    require(planning_request.isDefined)
    val customer = planning_request.get._1
    planning_request.get._2.forward_to_validate(customer,sol)
  }
  def reply_solutions(sol:Array[Solution]) = {
    require(planning_request.isDefined)
    planning_request.get._2.accept_as_unvalidated(sol)
  }
  def reply_no_solutions = {
    require(planning_request.isDefined)
    planning_request.get._2.empty_sol_set
  }

  def role__received_request_for_planning(sender: ActorRef, msg: RequestSolutions): Unit

  registerRole {
    case msg:RequestSolutions =>
      mylog("planning request")
      planning_request = Some(sender,msg)
      role__received_request_for_planning(sender,msg)
  }

}

trait SolutionValidator extends MUSARole {
  val sol_queue = new mutable.Queue[RequestToValidatePlans]()

  def reply_solutions(msg: RequestToValidatePlans,sol:Array[MetaSolInfo]):Unit = {
    msg.validated(sol)
  }
  def reply_no_solutions(msg: RequestToValidatePlans):Unit = {
    msg.empty_sol_set
  }

  def role__received_request_for_validation(sender: ActorRef, msg: RequestToValidatePlans): Unit

  registerRole {
    case msg:RequestToValidatePlans =>
      mylog("validation request")
      sol_queue.enqueue( msg )
      role__received_request_for_validation(sender,msg)
  }

}
