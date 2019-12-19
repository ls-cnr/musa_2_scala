package org.icar.musa.pmr

import org.icar.fol.Entail
import org.icar.ltl.supervisor.SupervisorBuilder
import org.icar.musa.context.StateOfWorld
import org.icar.musa.main_entity.{AbstractCapability, GroundedAbstractCapability}

class SingleGoalProblemExploration(ps:SingleGoalProblemSpecification, cap_set : Array[AbstractCapability], agent : String) {

  val su_builder = new SupervisorBuilder()
  val entail: Entail.type = Entail

  var to_visit: List[WTSStateNode] = List[WTSStateNode]()
  var visited: List[WTSStateNode] = List[WTSStateNode]()
  var expansions: List[WTSExpansion] = List[WTSExpansion]()

  /* for testing purposes */
  var iteration : Int = 0
  var focused : Option[WTSStateNode] = None
  var cap_that_applied: List[AbstractCapability] = List[AbstractCapability]()
  var w_produced: List[WTSStateNode] = List[WTSStateNode]()

  def new_node(node: WTSStateNode) : Unit = {
    if (!to_visit.contains(node) & !visited.contains(node))
      to_visit = node :: to_visit
    to_visit.sortBy( (x:WTSStateNode)=>x.qos )
  }

  def execute_iteration() : Unit = {
    val start_timestamp: Long = System.currentTimeMillis
    cap_that_applied = List[GroundedAbstractCapability]()      // TESTING PURPOSE
    w_produced = List[WTSStateNode]()                          // TESTING PURPOSE

    val current : Option[WTSStateNode] = pick_most_promising_node
    focused = current                                         // TESTING PURPOSE
    if (current.isDefined) {
      val node = current.get
      //println("current ["+node.caps+"]")
      visited = node :: visited

      val cap_results = entail.capabilities(node.w,ps.ass_set,cap_set)

      var counter = 1
      for (c <- cap_set if !node.caps.contains( c.name ) && !node.caps.contains( c.asInstanceOf[GroundedAbstractCapability].opposite ) && cap_results(c.name)) {

        val exp : Option[WTSExpansion] = generate_capability_evolution(node, c)
        if (exp.isDefined) {
          //println(counter+":"+ps.asset.pretty_string(exp.get))
          counter += 1
          expansions = exp.get :: expansions
        }

      }
      expansions = expansions.sortBy( _.order ).reverse

    }
    iteration += 1      // TESTING PURPOSE
  }

  def highest_expansion : Option[WTSExpansion] = expansions.headOption

  def pick_expansion(exp : WTSExpansion) : Unit = {
    expansions = expansions.filter(_ != exp)
  }

  private def pick_most_promising_node: Option[WTSStateNode] = {
    if (to_visit.isEmpty)
      None
    else {
      val head = to_visit.head
      to_visit = to_visit.tail
      Some( head )
    }
  }

  private def generate_capability_evolution(node: WTSStateNode, cap: AbstractCapability): Option[WTSExpansion] = {
    var res : Option[WTSExpansion] = None

    cap match {
      case c : GroundedAbstractCapability =>
        cap_that_applied = c :: cap_that_applied      // TESTING PURPOSE

        // only 1 scenario
        if (c.scenarios.size==1) {
          val w2 = StateOfWorld.extend(node.w,c.scenarios.head._2.evo)

          val su2 = su_builder.update(node.su,w2,ps.ass_set)
          val qos = ps.asset.evaluate_node(w2,su2.distance_to_satisfaction)

          val node2 = WTSStateNode(w2, su2, qos,  c.name :: node.caps )
          w_produced = node2 :: w_produced      // TESTING PURPOSE
          res = Some(SimpleWTSExpansion(node,node2,c,agent))

        // more than 1 scenario
        } else {
          var evo = Map[String,WTSStateNode]()
          var average_score : Float = 0
          var min_distance : Float = 10000

          for (scen <- c.scenarios.keys) {
            val op = c.scenarios(scen)
            val w2 = StateOfWorld.extend(node.w,op.evo)

            val su2 = su_builder.update(node.su,w2,ps.ass_set)
            val qos = ps.asset.evaluate_node(w2,su2.distance_to_satisfaction)

            average_score += qos

            min_distance = math.min(min_distance, su2.distance_to_satisfaction)

            val node2 = WTSStateNode(w2, su2, qos,   c.name :: node.caps)
            w_produced = node2 :: w_produced      // TESTING PURPOSE
            evo = evo + (scen -> node2)
          }
          average_score = average_score / c.scenarios.size

          res = Some(MultiWTSExpansion(node,evo,min_distance,average_score,c,agent))
        }
        res

      case _ => None
    }

  }

  def log_iteration() : Unit = {
    val q = ps.asset
    println("Iteration "+iteration)
    println("Focused "+q.pretty_string(focused.get))
    print("Cap that applied ("+cap_that_applied.size+"): ")
    cap_that_applied.foreach(c => print(c.name+","))
    println

    println("to visit States ("+to_visit.size+"): ")
    to_visit.foreach(w => println( q.pretty_string(w)) )

    val exp = highest_expansion
    val exp_string = if (exp.isDefined) q.pretty_string(exp.get) else "<empty>"

    println("Exp ("+expansions.size+")")
    expansions.foreach(w => println( q.pretty_string(w)) )
    println("best is: " + exp_string)

    println("--- end iteration ---")
  }


}

object SingleGoalProblemExploration {
  def apply(ps:SingleGoalProblemSpecification, w : StateOfWorld, cap_set : Array[AbstractCapability], agent : String ="no-agent") : SingleGoalProblemExploration = {
    val pe = new SingleGoalProblemExploration(ps,cap_set,agent)
    val su = pe.su_builder.initialize(ps.goal.ltl,w,ps.ass_set)
    val qos = ps.asset.evaluate_node(w,su.distance_to_satisfaction)
    val root = WTSStateNode(w, su, qos)
    pe.to_visit = root :: pe.to_visit
    pe
  }
}
