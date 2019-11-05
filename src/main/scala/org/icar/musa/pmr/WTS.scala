package org.icar.musa.pmr

import org.icar.fol._
import org.icar.ltl.{Finally, Globally, LogicAtom, LogicConjunction}
import org.icar.ltl.supervisor.NetSupervisor
import org.icar.musa.context.{Deprec_AddEvoOperator, EvoOperator, StateOfWorld}
import org.icar.musa.main_entity.{AbstractCapability, EvolutionScenario, GroundedAbstractCapability, LTLGoal}

class WTS (val root:WTSStateNode) {
  private var nodes: Set[WTSStateNode] = Set[WTSStateNode](root)
  private var arcs: Set[WTSCapabilityArc] = Set[WTSCapabilityArc]()
  private var multiarcs: Set[WTSScenarioArc] = Set[WTSScenarioArc]()

  def size: Int = nodes.size

  def addExpansion(exp : WTSExpansion) : Unit = {
    exp match {

      case simple : SimpleWTSExpansion =>
        nodes = nodes + simple.start
        nodes = nodes + simple.end
        arcs = arcs + WTSCapabilityArc(simple.start,simple.end,simple.cap,simple.prov)


      case multi : MultiWTSExpansion =>
        nodes = nodes + multi.start
        for (n <- multi.evo.values)
          nodes = nodes + n
        multiarcs = multiarcs + WTSScenarioArc(multi.start,multi.evo, multi.cap, multi.prov)
    }
  }

  def outgoing_capabilities(n : WTSStateNode) : List[WTSCapabilityArc] = {
    var l = List[WTSCapabilityArc]()

    for (a <- arcs if a.in == n)
      l = a :: l

    l
  }
  def incoming_capabilities(n : WTSStateNode) : List[WTSCapabilityArc] = {
    var l = List[WTSCapabilityArc]()

    for (a <- arcs if a.out == n)
      l = a :: l

    l
  }
  def outgoing_scenarios(n : WTSStateNode) : List[WTSScenarioArc] = {
    var l = List[WTSScenarioArc]()

    for (a <- multiarcs if a.in == n)
      l = a :: l

    l
  }

  def print_for_graphviz( pretty_string: StateOfWorld => String) : Unit = {
    println("digraph WTS {")
    for (n <- nodes.toList) {
      print("\""+pretty_string(n.w)+"\"")
      if (n==root)
        println(" [style=bold,color=yellow];")
      else if (n.su.isAccepted)
        println(" [style=bold,color=green];")
      else
        println(" [color=black];")
    }

    for (a <- arcs) {
      print("\""+pretty_string(a.in.w)+"\"")
      print(" -> ")
      print("\""+pretty_string(a.out.w)+"\"")
      println(" [label=\""+a.cap.name+"\"];")
    }

    var counter = 1
    for (a<-multiarcs) {
      //println("X"+counter)
      print("\""+pretty_string(a.in.w)+"\"")
      print(" -> ")
      print("X"+counter)
      println(" [label=\""+a.cap.name+"\"];")

      for (scenario <- a.out.keys) {
        print("X"+counter)
        print(" -> ")
        print("\""+pretty_string(a.out(scenario).w)+"\"")
        println(" [label=\""+scenario+"\"];")
      }
      counter = counter +1
    }
    println("}")
  }

  def to_graphviz_string( pretty_string: StateOfWorld => String) : String = {
    var string = "digraph WTS {\n"

    for (n <- nodes.toList) {
      string += "\""+pretty_string(n.w)+"\""
      if (n==root)
        string += "[style=bold,color=yellow];\n"
      else if (n.su.isAccepted)
        string += "[style=bold,color=green];\n"
      else
        string += "[color=black];\n"
    }

    for (a <- arcs) {
      string += "\""+pretty_string(a.in.w)+"\""
      string += "->"
      string += "\""+pretty_string(a.out.w)+"\""
      string += "[label=\""+a.cap.name+"\"];\n"
    }

    var counter = 1
    for (a<-multiarcs) {
      //println("X"+counter)
      string += "\""+pretty_string(a.in.w)+"\""
      string += "->"
      string += "X"+counter
      string += "[label=\""+a.cap.name+"\"];\n"

      for (scenario <- a.out.keys) {
        string += "X"+counter
        string += "->"
        string += "\""+pretty_string(a.out(scenario).w)+"\""
        string += "[label="+scenario+"];\n"
      }
      counter = counter +1
    }
    string += "}\n"

    string
  }

}


case class WTSStateNode(w:StateOfWorld, su : NetSupervisor, qos : Float, caps : List[String] = List.empty)
case class WTSCapabilityArc(in : WTSStateNode, out : WTSStateNode, cap : GroundedAbstractCapability, prov : String="empty")
case class WTSScenarioArc(in : WTSStateNode, out : Map[String,WTSStateNode], cap : GroundedAbstractCapability, prov : String="empty")

abstract class WTSExpansion(val order : Float)
case class SimpleWTSExpansion(start:WTSStateNode,end:WTSStateNode, cap:GroundedAbstractCapability,prov:String="empty") extends WTSExpansion(end.qos) {
  override def toString: String = "{("+start.w+" => "+end.w+" with "+cap.name+" ["+end.su.current_state+" dist="+end.su.distance_to_satisfaction+" ("+end.su.petrinets+")}"
}
case class MultiWTSExpansion(start:WTSStateNode, evo: Map[String,WTSStateNode], average_distance_to_sat: Float, average_qos:Float, cap:GroundedAbstractCapability, prov:String="empty") extends WTSExpansion(average_qos) {
  override def toString: String = {
    var end =""
    for (e <- evo)
      end += e._2+"->"+e._2.w+"["+e._2.su.current_state+"("+e._2.su.distance_to_satisfaction+")],"

    "{from("+start.w+" to ("+end+") with "+cap.name+" (score="+average_qos+")}"
  }
}

object SimpleWTSExpansion {
  def init_for_test : SimpleWTSExpansion = {
    val w = StateOfWorld.create(GroundPredicate("receipt",AtomTerm("doc")))

    val f1 = Globally(LogicAtom("document",AtomTerm("doc")))
    val f2 = Finally(LogicAtom("ready",AtomTerm("doc")))
    val f3 = LogicConjunction(f1,f2)
    val goal = LTLGoal(f3)

    val ass = AssumptionSet( Assumption("document(X) :- receipt(X).")  )
    val quality = new EmptyQualityAsset(ass)

    val ps = SingleGoalProblemSpecification(ass,goal,quality)

    val pre = FOLCondition(Literal(Predicate("document", AtomTerm("doc"))))
    val post = FOLCondition(Literal(Predicate("ready", AtomTerm("doc"))))
    val evo = EvolutionScenario(Array[EvoOperator](Deprec_AddEvoOperator(GroundPredicate("ready", AtomTerm("doc")))))
    val c1 = GroundedAbstractCapability("working_the_doc",pre,post,Map("1"-> evo))
    val cap_set = Array[AbstractCapability](c1)

    val expl = SingleGoalProblemExploration(ps, w, cap_set)
    expl.execute_iteration()

    expl.highest_expansion.get.asInstanceOf[SimpleWTSExpansion]
  }
}
object MultiWTSExpansion {
  def apply(start:WTSStateNode, evo: scala.collection.Map[String,WTSStateNode], average_distance_to_sat: Float, average_qos:Float, cap:GroundedAbstractCapability, prov:String) : MultiWTSExpansion =
    new MultiWTSExpansion(start,evo.toMap, average_distance_to_sat,average_qos,cap,prov)

  def init_for_test : MultiWTSExpansion = {
    val w = StateOfWorld.create(GroundPredicate("receipt",AtomTerm("doc")))

    val f1 = Globally(LogicAtom("document",AtomTerm("doc")))
    val f2 = Finally(LogicAtom("ready",AtomTerm("doc")))
    val f3 = LogicConjunction(f1,f2)
    val goal = LTLGoal(f3)

    val ass = AssumptionSet( Assumption("document(X) :- receipt(X).")  )
    val quality = new EmptyQualityAsset(ass)

    val ps = SingleGoalProblemSpecification(ass,goal,quality)

    val pre = FOLCondition(Literal(Predicate("document", AtomTerm("doc"))))
    val post = FOLCondition(Literal(Predicate("ready", AtomTerm("doc"))))
    val evo1 = EvolutionScenario(Array[EvoOperator](Deprec_AddEvoOperator(GroundPredicate("ready", AtomTerm("doc")))))
    val evo2 = EvolutionScenario(Array[EvoOperator](Deprec_AddEvoOperator(GroundPredicate("unready", AtomTerm("doc")))))
    val c1 = GroundedAbstractCapability("working_the_doc",pre,post,Map("1"-> evo1, "2"->evo2))
    val cap_set = Array[AbstractCapability](c1)

    val expl = SingleGoalProblemExploration(ps, w, cap_set)
    expl.execute_iteration()

    expl.highest_expansion.get.asInstanceOf[MultiWTSExpansion]
  }
}




