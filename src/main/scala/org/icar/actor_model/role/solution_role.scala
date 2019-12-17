package org.icar.actor_model.role

import akka.actor.ActorRef
import org.icar.actor_model.{MUSARole, MetaSolInfo}
import org.icar.actor_model.protocol.AbstractSolProtocol
import org.icar.actor_model.protocol.AbstractSolProtocol.{InformEmptySet, InformSolutions, RequestSolutions, RequestToValidatePlans}
import org.icar.pmr_solver.high_level_specification.LTLGoalSet
import org.icar.pmr_solver.symbolic_level.RawState

import scala.collection.mutable
import scala.org.icar.high_level_specification.Solution

trait SolutionCustomer extends MUSARole {
  def request_solutions_to_problem(initial_state:RawState,goal_set:LTLGoalSet) = AbstractSolProtocol.init(initial_state,goal_set)

  def received_abstract_solutions(sender: ActorRef, msg: InformSolutions): Unit
  def received_empty_solutions(sender: ActorRef, msg: InformEmptySet): Unit

  override def role_description: Receive = {
    case msg:InformSolutions =>
      received_abstract_solutions(sender,msg)
    case msg:InformEmptySet =>
      received_empty_solutions(sender,msg)
  }

}

trait SolutionProducer extends MUSARole {
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

  def received_request_for_planning(sender: ActorRef, msg: RequestSolutions): Unit

  override def role_description: Receive = {
    case msg:RequestSolutions =>
      planning_request = Some(sender,msg)
      received_request_for_planning(sender,msg)
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

  def received_request_for_validation(sender: ActorRef, msg: RequestToValidatePlans): Unit

  override def role_description: Receive = {
    case msg:RequestToValidatePlans =>
      sol_queue.enqueue( msg )
      received_request_for_validation(sender,msg)
  }

}
