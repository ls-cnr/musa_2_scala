package org.icar.musa

import org.icar.high_level_specification.{AbstractCapability, CapabilityGrounding, StateOfWorld}
import org.icar.pmr_solver.symbolic_level.{RawAction, RawState, RawTT}
import org.scalatest.FunSuite

import scala.org.icar.high_level_specification._
import scala.org.icar.pmr_solver.best_first_planner.{RawWTSArc, StateLabel, WTSGraph, WTSLabelling}

class WTS_suit extends FunSuite {

	test("WTS should be converted into a Solution") {
		val A=RawAction("actA",RawTT(),Array(),List(),CapabilityGrounding(AbstractCapability.empty("A"),Map.empty))
		val B=RawAction("actB",RawTT(),Array(),List(),CapabilityGrounding(AbstractCapability.empty("B"),Map.empty))
		val C=RawAction("actC",RawTT(),Array(),List(),CapabilityGrounding(AbstractCapability.empty("C"),Map.empty))
		val D=RawAction("actD",RawTT(),Array(),List(),CapabilityGrounding(AbstractCapability.empty("D"),Map.empty))
		val E=RawAction("actE",RawTT(),Array(),List(),CapabilityGrounding(AbstractCapability.empty("E"),Map.empty))
		val F=RawAction("actF",RawTT(),Array(),List(),CapabilityGrounding(AbstractCapability.empty("F"),Map.empty))
		val w0=RawState(Array(false,false,true)); val w1=RawState(Array(false,true,false))
		val w2=RawState(Array(false,true,true)); val w3=RawState(Array(true,false,false))
		val w4=RawState(Array(true,false,true)); val w5=RawState(Array(true,true,false))

		val nodes = Set(w0,w1,w2,w3,w4,w5)
		val transitions = Set(
			RawWTSArc(w0,w1,1,A,""),
			RawWTSArc(w1,w2,1,B,""),
			RawWTSArc(w2,w3,1,C,"case1"),
			RawWTSArc(w3,w1,1,D,""),
			RawWTSArc(w2,w4,1,E,"case2"),
			RawWTSArc(w4,w5,1,F,""),
		)
		val nodes_labelling : Map[RawState,StateLabel] = Map (
			w0 -> StateLabel(null,false,false,false,true,0),
			w1 -> StateLabel(null,false,false,false,true,0),
			w2 -> StateLabel(null,false,false,false,true,0),
			w3 -> StateLabel(null,false,false,false,true,0),
			w4 -> StateLabel(null,false,false,false,true,0),
			w5 -> StateLabel(null,true,false,true,true,0),
		)
		val wts_label = WTSLabelling(nodes_labelling,0,List.empty,true)
		val wts = WTSGraph(w0,nodes,transitions,Set.empty,wts_label)

		def testCompleteWTS_to_Solution (): Unit = {

			val sol = WTSGraph.WTStoSolution(wts,StateOfWorld(List.empty))
			assert(sol.complete)
			assert(sol.wfitems.size==10)
			assert(sol.wfflow.size ==10)

			val flow1 = sol.wfflow.filter(_.from==StartEvent())
			assert(flow1.size==1)
			val t1 = flow1.head.to
			assert(t1.isInstanceOf[Task])
			assert(t1.asInstanceOf[Task].grounding.capability.id=="A")

			val flow2 = sol.wfflow.filter(_.from==t1)
			assert(flow2.size==1)
			val j1 = flow2.head.to
			assert(j1.isInstanceOf[JoinGateway])

			val flow3 = sol.wfflow.filter(_.from==j1)
			assert(flow3.size==1)
			val t3 = flow3.head.to
			assert(t3.isInstanceOf[Task])
			assert(t3.asInstanceOf[Task].grounding.capability.id=="B")

			val flow4 = sol.wfflow.filter(_.from==t3)
			assert(flow4.size==1)
			val s1 = flow4.head.to
			assert(s1.isInstanceOf[SplitGateway])

			val flow5 = sol.wfflow.filter(_.from==s1)
			assert(flow5.size==2)
			val t4 = flow5.head.to
			val t5 = flow5.tail.head.to
			assert(t4.isInstanceOf[Task])
			assert(t4.asInstanceOf[Task].grounding.capability.id=="E" || t4.asInstanceOf[Task].grounding.capability.id=="C")
			assert(t5.isInstanceOf[Task])
			assert(t5.asInstanceOf[Task].grounding.capability.id=="E" || t5.asInstanceOf[Task].grounding.capability.id=="C")

			val flow6 =
				if (t4.asInstanceOf[Task].grounding.capability.id=="C")
					sol.wfflow.filter(_.from==t4)
				else
					sol.wfflow.filter(_.from==t5)
			assert(flow6.size==1)
			val t6 = flow6.head.to
			assert(t6.isInstanceOf[Task])
			assert(t6.asInstanceOf[Task].grounding.capability.id=="D")

			val flow7 = sol.wfflow.filter(_.from==t6)
			assert(flow7.size==1)
			val j2 = flow7.head.to
			assert(j2.isInstanceOf[JoinGateway])
			assert(j2==j1)


			val flow8 =
				if (t4.asInstanceOf[Task].grounding.capability.id=="E")
					sol.wfflow.filter(_.from==t4)
				else
					sol.wfflow.filter(_.from==t5)
			assert(flow8.size==1)
			val t7 = flow8.head.to
			assert(t7.isInstanceOf[Task])
			assert(t7.asInstanceOf[Task].grounding.capability.id=="F")

			val flow9 = sol.wfflow.filter(_.from==t7)
			assert(flow9.size==1)
			val end = flow9.head.to
			assert(end.isInstanceOf[EndEvent])

			println(sol.to_graphviz())
		}

	}

}
