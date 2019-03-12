package org.icar.musa.scenarios.sps

import java.rmi.registry.LocateRegistry
import java.util

import matMusa.MatRemote
import org.icar.musa.pmr._
import org.icar.musa.scenarios.SPSScenario
import org.icar.musa.specification.ValidationStrategy

class SPSValidationStrategy(domain : SPSScenario) extends ValidationStrategy {
  var stub: MatRemote = null

  val host = "194.119.214.137"
  val port = 1099

  init

  private def init(): Unit = {
    println("Connecting to Matlab...")
    try {
      val registry = LocateRegistry.getRegistry(host,port)
      //println("get RMI registry")
      stub = registry.lookup("MatRemote").asInstanceOf[MatRemote]
      //println("get stub")
      //stub.startEngine()
      println("Engine Matlab discovered")
    }
    catch {
      case e: Exception =>
        System.err.println("Client exception: " + e.toString)
        e.printStackTrace()
    }
  }

  private def validate_well_formedness(sol: Solution, item : WfItem, visited : List[WfItem]): Boolean = {
    if (!visited.contains(item)) {
      val out = sol.arcs_out_from(item)
      if (out.length<0 || out.length>1)
        false

      else
        item match {
          case x:WfStartEvent => validate_well_formedness(sol, out(0).to , x :: visited )
          case x:WfTask => validate_well_formedness(sol, out(0).to , x :: visited )
          case x:WfEndEvent => true
          case _ => false
        }

    } else

      false
  }

  override def validate(sol: Solution): Boolean = {
    var result: Boolean = false

    val w : Boolean = validate_well_formedness(sol,sol.start,List())

    if (w && stub != null) {
      val solution_for_matlab: util.ArrayList[String] = EvaluateSol.solution_list(sol)
      val all_switchers : util.ArrayList[String] = new util.ArrayList()
      for (s <- domain.circuit.switcher)
        all_switchers.add(s.id.toLowerCase)
      val open_switchers : util.ArrayList[String] = new util.ArrayList()
      for (s<-domain.scenario.open_switchers)
        open_switchers.add(s)

      //println("ALL SWITCHERS")
      //println(all_switchers)

      try {
        result = stub.evaluateSolution(solution_for_matlab, all_switchers, open_switchers)
        println("Result = " + result)
      } catch {
        case e: Exception =>
          println("Client exception: " + e.toString)
          e.printStackTrace()
      }
    }
    result
  }

}
