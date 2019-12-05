package scala.org.icar.high_level_specification

import java.io.{File, PrintWriter}

import org.icar.pmr_solver.high_level_specification.{AbstractCapability, CapGrounding, ConstantTerm, DomainArgument, HL_PredicateFormula, StateOfWorld, True}
import org.icar.pmr_solver.symbolic_level.RawState

abstract class WorkflowItem
case class StartEvent() extends WorkflowItem
case class EndEvent() extends WorkflowItem
case class Task(id:Int,grounding : CapGrounding) extends WorkflowItem
case class SplitGateway(id:Int,outport:List[String]) extends WorkflowItem
case class JoinGateway(id:Int) extends WorkflowItem

case class SequenceFlow(from:WorkflowItem,to:WorkflowItem,scenario:String="",condition:HL_PredicateFormula=True())


case class Solution(
	                   start:StateOfWorld,
	                   wfitems: Array[WorkflowItem],
	                   wfflow:Array[SequenceFlow],
	                   complete: Boolean
					) {

	private def print_item(n: WorkflowItem): String = {
		n match {
			case StartEvent() => "start"
			case EndEvent() => "end"
			case Task(_, grounding) => grounding.unique_id
			case JoinGateway(id) => "J"+id
			case SplitGateway(id, outport) => "S"+id
		}
	}
	private def print_item_decoration(n: WorkflowItem): String = {
		n match {
			case StartEvent() => "[shape=doublecircle,color=black];\n"
			case EndEvent() => "[shape=doublecircle,color=green];\n"
			case Task(_, _) => "[shape=box,color=black];\n"
			case JoinGateway(_) => "[shape=diamond,color=black];\n"
			case SplitGateway(_, _) => "[shape=diamond,color=black];\n"
		}
	}

	def to_graphviz() : String = {
		var string = "digraph Solution {\n"+"rankdir=LR\n"+"{rank=min; \"start\"}\n"+"{rank=max; \"end\"}\n"

		for (n <- wfitems)
			string += "\""+print_item(n)+"\""+print_item_decoration(n)

		for (f <- wfflow) {
			string += "\""+print_item(f.from)+"\""
			string += "->"
			string += "\""+print_item(f.to)+"\""
			string += "[label=\""+f.scenario+"\"];\n"
		}
		string + "}\n"
	}

	def update_wts_file(file:String) : Unit = {
		val pw = new PrintWriter(new File(file))
		pw.write(to_graphviz())
		pw.close
	}

}


