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
  var w_produced: List[WTSStateNode] = List[WTSStateNode]()

  private def init() : WTSStateNode = {
    val su = NetSupervisor.initialize(ps.goal.ltl,w,ps.ass_set)
    val qos = ps.asset.evaluate_node(w,su.distance_to_satisfaction)
    val root = WTSStateNode(w, su, qos)
    to_visit = root :: to_visit

    root
  }

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
      visited = node :: visited

      //val before_entail_timestamp: Long = System.currentTimeMillis
      val cap_results = Entail.capabilities(node.w,ps.ass_set,cap_set)
      //val after_entail_timestamp: Long = System.currentTimeMillis

      for (c <- cap_set if cap_results(c.name)) {
        val exp : Option[WTSExpansion] = generate_capability_evolution(node, c)
        if (exp.isDefined) {
          expansions = exp.get :: expansions
        }
      }
      //val after_expand_timestamp: Long = System.currentTimeMillis
      expansions = expansions.sortBy( _.order ).reverse
      //val after_sort_timestamp: Long = System.currentTimeMillis
      //println("entail ms: "+(after_entail_timestamp-before_entail_timestamp))
      //println("expand ms: "+(after_expand_timestamp-after_entail_timestamp))
      //println("sort ms: "+(after_sort_timestamp-after_expand_timestamp))
       /* (x:WTSExpansion)=>
          x match {
            case a: SimpleWTSExpansion => a.end.qos
            case b: MultiWTSExpansion => b.average_qos
          } )*/

    }
    iteration += 1      // TESTING PURPOSE
    val end_timestamp: Long = System.currentTimeMillis
    println("iteration ms: "+(end_timestamp-start_timestamp))
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
          //val before_extend_timestamp: Long = System.currentTimeMillis
          val w2 = StateOfWorld.extend(node.w,c.scenarios.head._2.evo)
          //val after_extend_timestamp: Long = System.currentTimeMillis

          val su2 = NetSupervisor.update(node.su,w2,ps.ass_set)
          //val after_su_update_timestamp: Long = System.currentTimeMillis
          val qos = ps.asset.evaluate_node(w2,su2.distance_to_satisfaction)
          //val after_qos_evaluate_timestamp: Long = System.currentTimeMillis

          //println("state extend ms: "+(after_extend_timestamp-before_extend_timestamp))
          //println("su update ms: "+(after_su_update_timestamp-after_extend_timestamp))
          //println("qos update ms: "+(after_qos_evaluate_timestamp-after_su_update_timestamp))

          val node2 = WTSStateNode(w2, su2, qos)
          w_produced = node2 :: w_produced      // TESTING PURPOSE
          Some(SimpleWTSExpansion(node,node2,c,agent))

        // more than 1 scenario
        } else {
          var evo = Map[String,WTSStateNode]()
          var average_score : Float = 0
          var min_distance : Float = 10000

          for (scen <- c.scenarios.keys) {
            val op = c.scenarios(scen)
            val w2 = StateOfWorld.extend(node.w,op.evo)

            val su2 = NetSupervisor.update(node.su,w2,ps.ass_set)
            val qos = ps.asset.evaluate_node(w2,su2.distance_to_satisfaction)

            average_score += qos

            min_distance = math.min(min_distance, su2.distance_to_satisfaction)

            val node2 = WTSStateNode(w2, su2, qos)
            w_produced = node2 :: w_produced      // TESTING PURPOSE
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

    //println("produced States ("+w_produced.size+"): ")
    //w_produced.foreach(w => println( q.pretty_string(w)) )

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
