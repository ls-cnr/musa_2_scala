package org.icar.musa.pmr

import org.icar.fol.Entail
import org.icar.ltl.supervisor.NetSupervisor
import org.icar.musa.context.StateOfWorld
import org.icar.musa.spec.{AbstractCapability, GroundedAbstractCapability}

class SingleGoalProblemExploration(ps:SingleGoalProblemSpecification, w:StateOfWorld, cap_set : Array[AbstractCapability], agent : String ="no-agent") {

  var to_visit: List[WTSStateNode] = List[WTSStateNode]()
  var visited: List[WTSStateNode] = List[WTSStateNode]()
  var expansions: List[WTSExpansion] = List[WTSExpansion]()

  val root: WTSStateNode = init()

  /* for testing purposes */
  var iteration : Int = 0
  var focused : Option[WTSStateNode] = None
  var cap_that_applied: List[AbstractCapability] = List[AbstractCapability]()
  var w_produced: List[StateOfWorld] = List[StateOfWorld]()

  private def init() : WTSStateNode = {
    val su = NetSupervisor.initialize(ps.goal.ltl,w,ps.ass_set)
    val root = WTSStateNode(w,su, ps.asset.evaluate_state(w))
    to_visit = root :: to_visit

    root
  }

  def new_node(node: WTSStateNode) : Unit = {
    if (!to_visit.contains(node) & !visited.contains(node))
      to_visit = node :: to_visit
    to_visit.sortBy( (x:WTSStateNode)=>x.su.distance_to_satisfaction )
  }

  def execute_iteration() : Unit = {
    cap_that_applied = List[GroundedAbstractCapability]()      // TESTING PURPOSE
    w_produced = List[StateOfWorld]()                          // TESTING PURPOSE

    val current : Option[WTSStateNode] = pick_most_promising_node
    focused = current                                         // TESTING PURPOSE
    if (current.isDefined) {
      val node = current.get
      visited = node :: visited

      val cap_results = Entail.capabilities(node.w,ps.ass_set,cap_set)

      for (c <- cap_set if cap_results(c.name)) {
        val exp : Option[WTSExpansion] = generate_capability_evolution(node, c)
        if (exp.isDefined) {
          expansions = exp.get :: expansions
          expansions.sortBy( (x:WTSExpansion)=> x match { case a: SimpleWTSExpansion => a.end.su.distance_to_satisfaction; case b: MultiWTSExpansion => b.distance_to_satisfaction} )
        }
      }

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
    cap match {
      case c : GroundedAbstractCapability =>
        cap_that_applied = c :: cap_that_applied      // TESTING PURPOSE

        // only 1 scenario
        if (c.scenarios.size==1) {
          val w2 = StateOfWorld.extend(node.w,c.scenarios.head._2.evo)
          w_produced = w2 :: w_produced      // TESTING PURPOSE

          val su2 = NetSupervisor.update(node.su,w2,ps.ass_set)
          val node2 = WTSStateNode(w2,su2, ps.asset.evaluate_state(w2))
          Some(SimpleWTSExpansion(node,node2,c,agent))

        // more than 1 scenario
        } else {
          var evo = Map[String,WTSStateNode]()
          var average_score : Float = 0
          var min_distance : Float = 10000

          for (scen <- c.scenarios.keys) {
            val op = c.scenarios(scen)
            val w2 = StateOfWorld.extend(node.w,op.evo)
            val qos = ps.asset.evaluate_state(w2)
            w_produced = w2 :: w_produced      // TESTING PURPOSE

            val su2 = NetSupervisor.update(node.su,w2,ps.ass_set)

            average_score += qos
            min_distance = math.min(min_distance, su2.distance_to_satisfaction)

            val node2 = WTSStateNode(w2,su2, ps.asset.evaluate_state(w2))
            evo = evo + (scen -> node2)
          }
          average_score = average_score / c.scenarios.size

          Some(MultiWTSExpansion(node,evo,min_distance,average_score,c,agent))
        }

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

    print("States ("+w_produced.size+"): ")
    w_produced.foreach(w => println( q.pretty_string(w)) )

    val exp = highest_expansion
    val exp_string = if (exp.isDefined) q.pretty_string(exp.get) else "<empty>"

    println("Exp ("+expansions.size+"), best is: " + exp_string)

    println("--- end iteration ---")
  }


}
