package org.icar.musa.scenarios

import org.icar.fol._
import org.icar.ltl.{Finally, LogicConjunction, ltlFormula}
import org.icar.musa.context.{Deprec_AddEvoOperator, EvoOperator, Deprec_RemoveEvoOperator, StateOfWorld}
import org.icar.musa.pmr._
import org.icar.musa.scenarios.sps.{Circuit, Mission, ReconfigurationScenario}
import org.icar.musa.main_entity._

import scala.collection.mutable.ArrayBuffer

class SPSScenario(path:String) extends Scenario {

//  val circuit: Circuit = Circuit.load_from_file(path + "/small_circuit.txt")
//  val scenario: ReconfigurationScenario = ReconfigurationScenario.scenario_small_1 //scenario_circuit3_parsed_1
//  val mission: Mission = Mission.mission_small_1 //circuit3_file_mission_1

  val circuit: Circuit = Circuit.load_from_file(path + "/circuit3.txt")
  val scenario: ReconfigurationScenario = ReconfigurationScenario.scenario_circuit3_parsed_1
  val mission: Mission = Mission.circuit3_file_mission_1

  override def assumption_set : AssumptionSet = {
    /* standard assumptions */
    val list = ArrayBuffer[Assumption]()
    list += Assumption("off(X) :- load(X), not on(X).")
    list += Assumption("off(X) :- generator(X), not on(X).")
    list += Assumption("down(X) :- node(X), not up(X).")
    list += Assumption("open(X) :- sw(X), not closed(X).")

    for (n<-circuit.nodes)
      list += Assumption("node(n"+n.id+").")

    for (c<-circuit.connections) {
      list += Assumption(c.source.up+":-"+c.dest.up+","+"not "+c.failure+".")
      list += Assumption(c.dest.up+":-"+c.source.up+","+"not "+c.failure+".")
    }
    for (s<-circuit.switcher) {
      list += Assumption("sw("+s.id+").")
      list += Assumption(s.source.up+":-"+s.dest.up+", "+s.closed+".")
      list += Assumption(s.dest.up+":-"+s.source.up+", "+s.closed+".")
    }
    for (g<-circuit.generators) {
      list += Assumption("generator("+g.id+").")
      list += Assumption(g.node.up+":-"+g.up+".")
    }
    for (l<-circuit.loads) {
      list += Assumption("load("+l.id+").")
      list += Assumption(l.up+":-"+l.node.up+".")
    }

    AssumptionSet(list: _*)
  }

  def axioms_set : Array[Assumption] = {
    /* standard assumptions */
    val buffer = ArrayBuffer[Assumption]()
    buffer += Assumption("off(X) :- load(X), not on(X).")
    buffer += Assumption("off(X) :- generator(X), not on(X).")
    buffer += Assumption("down(X) :- node(X), not up(X).")
    buffer += Assumption("open(X) :- sw(X), not closed(X).")

    for (n<-circuit.nodes)
      buffer += Assumption("node(n"+n.id+").")

    for (c<-circuit.connections) {
      buffer += Assumption(c.source.up+":-"+c.dest.up+","+"not "+c.failure+".")
      buffer += Assumption(c.dest.up+":-"+c.source.up+","+"not "+c.failure+".")
    }
    for (s<-circuit.switcher) {
      buffer += Assumption("sw("+s.id+").")
      buffer += Assumption(s.source.up+":-"+s.dest.up+", "+s.closed+".")
      buffer += Assumption(s.dest.up+":-"+s.source.up+", "+s.closed+".")
    }
    for (g<-circuit.generators) {
      buffer += Assumption("generator("+g.id+").")
      buffer += Assumption(g.node.up+":-"+g.up+".")
    }
    for (l<-circuit.loads) {
      buffer += Assumption("load("+l.id+").")
      buffer += Assumption(l.up+":-"+l.node.up+".")
    }

    buffer.toArray
  }

  override def goal_specification: LTLGoal = {
    var vitals = ArrayBuffer[ltlFormula]()
    for (v <- circuit.loads if mission.vitals.contains(v.id))
      vitals += v.atom
    val conj = LogicConjunction(vitals)
    LTLGoal(Finally(conj))
  }

  override def quality_asset: QualityAsset = {

    lazy val assumptions = assumption_set

    class SPSQualityAsset extends QualityAsset {
      val entail: Entail.type = Entail
      override def evaluate_node(w: StateOfWorld, goal_sat: Float): Float = evaluate_state(w).get
      override def evaluate_state(w: StateOfWorld): Option[Float] = {
        val cond_map = circuit.cond_map
        val res_map : Map[String, Boolean] = entail.condition_map(w,assumptions,cond_map)

        val node_map = circuit.node_map
        val node_res_map : Map[String, Boolean] = entail.condition_map(w,assumptions,node_map)
        var num_node_up=0
        for (n<-node_res_map.values if n==true)
          num_node_up += 1

        var supplied_pow : Float = 0
        for (g <- circuit.generators if res_map(g.id)) supplied_pow += mission.gen_pow(g.id)

        var residue_pow : Float = supplied_pow
        var absorbed_pow : Float = 0
        var not_enough_pow : Float = 0
        var digits : String = ""
        var max_digits = ""

        for (l_name <- mission.vitals) {
          max_digits += "1"
          if (res_map(l_name)) {
            digits += "1"

            absorbed_pow += mission.vital_pow
            if (residue_pow>mission.vital_pow) {
              //digits += "1"
              residue_pow -= mission.vital_pow
            } else {
              //digits += "0"
              not_enough_pow += mission.vital_pow
            }
          } else digits += "0"

        }

        for (l_name <- mission.semivitals) {
          max_digits += "1"
          if (res_map(l_name)) {
            digits += "1"

            absorbed_pow += mission.semivital_pow
            if (residue_pow>mission.semivital_pow) {
              //digits += "1"
              residue_pow -= mission.semivital_pow
            } else {
              //digits += "0"
              not_enough_pow += mission.semivital_pow
            }
          } else digits += "0"

        }

        for (l_name <- mission.nonvitals) {
          max_digits += "1"

          if (res_map(l_name)) {
            digits += "1"

            absorbed_pow += mission.nonvital_pow
            if (residue_pow>mission.nonvital_pow) {
              //digits += "1"
              residue_pow -= mission.nonvital_pow
            } else {
              //digits += "0"
              not_enough_pow += mission.nonvital_pow
            }
          } else digits += "0"

        }

        val pr_norm = Integer.parseInt(digits, 2).toDouble / Integer.parseInt(max_digits, 2).toDouble
        val nu_norm = if (supplied_pow>0) residue_pow / supplied_pow else 0
        val ne_norm = if (supplied_pow>0) not_enough_pow / supplied_pow else 0
        val u_norm = num_node_up.toDouble / circuit.nodes.size.toDouble

        val PR = 4*pr_norm
        val UP = 0.5*u_norm
        val NU = 0.5*nu_norm
        val NE = 4*ne_norm

        val m = (PR+UP)-(NU+NE)

        //println(s" Pr=$c1  NUp=$c2   NouUsed=$c3   NotEnough=$c4  => $m  ")


        Some( m.toFloat )
      }

      override def max_score: Float = math.pow(2, circuit.loads.length).toFloat

      override def pretty_string(w: StateOfWorld): String = {
        val cond_map = circuit.cond_map
        val res_map : Map[String, Boolean] = entail.condition_map(w,assumptions,cond_map)
        var digits : String = "["
        for (g <- circuit.generators)
          if (res_map(g.id)) digits+="1" else digits+="0"
        digits += " | "
        for (vital <- mission.vitals)
          if (res_map(vital)) digits+="1" else digits+="0"
        digits += " "
        for (semivital <- mission.semivitals)
          if (res_map(semivital)) digits+="1" else digits+="0"
        digits += " "
        for (nonvital <- mission.nonvitals)
          if (res_map(nonvital)) digits+="1" else digits+="0"

        //        for (l <- circuit.loads)
//          if (res_map(l.id)) digits+="1" else digits+="0"
        digits += "]"
        digits
      }

      override def pretty_string(node: WTSStateNode): String = {
        "n("+pretty_string(node.w)+","+node.qos+")"
      }

      override def pretty_string(exp: WTSExpansion): String = {
        exp match {
          case s : SimpleWTSExpansion =>
            "x("+pretty_string(s.start.w)+"->"+pretty_string(s.end.w)+","+s.order+","+s.cap.name+")"
          case m : MultiWTSExpansion =>
            m.toString
        }
      }
    }

    new SPSQualityAsset()
  }

  override def initial_state : StateOfWorld = {
    var list = ArrayBuffer[GroundPredicate]()

    for (r <- circuit.switcher) {
      val state = if (scenario.open_switchers.contains(r.id)) "open" else "closed"
      list += GroundPredicate(state,AtomTerm(r.id))
    }

    for (r<- circuit.generators) {
      val state = if (scenario.up_generators.contains(r.id)) "on" else if (scenario.generator_malfunctioning.contains(r.id)) "error" else "off"
      list += GroundPredicate(state,AtomTerm(r.id))
    }

    for (r <- scenario.failures) {
      list += GroundPredicate("f",AtomTerm(r))
    }

    StateOfWorld.create(list.toArray)
  }

  override def capabilities : Array[AbstractCapability] = {
    var cap_list = ArrayBuffer[AbstractCapability]()

    for (gen <- circuit.generators if !scenario.generator_malfunctioning.contains(gen)) {
      //cap_list += generate_switch_on_generator(gen.id)
      //cap_list += generate_switch_off_generator(gen.id)
    }

    for (sw <- circuit.switcher if !scenario.switcher_malfunctioning.contains(sw)) {
      if (circuit.sw_map.contains(sw.id)) {
        val g2_name = circuit.sw_map(sw.id)
        cap_list += generate_combinated_on_off_switcher(sw.id,g2_name)
      } else {

        val parts = sw.id.split("switch")
        val second = parts(1)
        if (!second.startsWith("f")) {
          cap_list += generate_close_switcher(sw.id)
          cap_list += generate_open_switcher(sw.id)
        }
      }
    }


    cap_list.toArray
  }

  private def generate_switch_on_generator(name : String) : GroundedAbstractCapability = {
    val generator_name = "switch_ON_"+name
    val generator = AtomTerm(name)
    val pre = FOLCondition(Literal(Predicate("off", generator )))
    val post = FOLCondition(Literal(Predicate("on", generator )))
    val evo_1 = EvolutionScenario(Array[EvoOperator](Deprec_RemoveEvoOperator(GroundPredicate("off", generator)),Deprec_AddEvoOperator(GroundPredicate("on", generator))))
    GroundedAbstractCapability(generator_name,pre,post,Map("1"-> evo_1),DataInSpecification(ArrayBuffer()),DataOutSpecification(ArrayBuffer()),Map(),"switch_OFF_"+name)
  }

  private def generate_switch_off_generator(name : String) : GroundedAbstractCapability = {
    val generator_name = "switch_OFF_"+name
    val generator = AtomTerm(name)
    val pre = FOLCondition(Literal(Predicate("on", generator )))
    val post = FOLCondition(Literal(Predicate("off", generator )))
    val evo_1 = EvolutionScenario(Array[EvoOperator](Deprec_RemoveEvoOperator(GroundPredicate("on", generator)),Deprec_AddEvoOperator(GroundPredicate("off", generator))))
    GroundedAbstractCapability(generator_name,pre,post,Map("1"-> evo_1),DataInSpecification(ArrayBuffer()),DataOutSpecification(ArrayBuffer()),Map(),"switch_ON_"+name)
  }
  private def generate_close_switcher(name : String) : GroundedAbstractCapability = {
    val capname = "CLOSE_"+name
    val switcher = AtomTerm(name)
    val pre = FOLCondition(Literal(Predicate("open", switcher )))
    val post = FOLCondition(Literal(Predicate("closed", switcher )))
    val evo_1 = EvolutionScenario(Array[EvoOperator](Deprec_RemoveEvoOperator(GroundPredicate("open", switcher)),Deprec_AddEvoOperator(GroundPredicate("closed", switcher))))
    GroundedAbstractCapability(capname,pre,post,Map("1"-> evo_1),DataInSpecification(ArrayBuffer()),DataOutSpecification(ArrayBuffer()),Map(),"OPEN_"+name)
  }
  private def generate_open_switcher(name : String) : GroundedAbstractCapability = {
    val capname = "OPEN_"+name
    val switcher = AtomTerm(name)
    val pre = FOLCondition(Literal(Predicate("closed", switcher )))
    val post = FOLCondition(Literal(Predicate("open", switcher )))
    val evo_1 = EvolutionScenario(Array[EvoOperator](Deprec_RemoveEvoOperator(GroundPredicate("closed", switcher)),Deprec_AddEvoOperator(GroundPredicate("open", switcher))))
    GroundedAbstractCapability(capname,pre,post,Map("1"-> evo_1),DataInSpecification(ArrayBuffer()),DataOutSpecification(ArrayBuffer()),Map(),"CLOSE_"+name)
  }
  private def generate_combinated_on_off_switcher(name1 : String, name2 : String) : GroundedAbstractCapability = {
    val capname = "CLOSE_"+name1+"_&_OPEN_"+name2
    val switcher1 = AtomTerm(name1)
    val switcher2 = AtomTerm(name2)
    val pre = FOLCondition(Conjunction(Literal(Predicate("open", switcher1 )),Literal(Predicate("closed", switcher2 ))))
    val post = FOLCondition(Conjunction(Literal(Predicate("closed", switcher1 )),Literal(Predicate("open", switcher2 ))))
    val evo_1 = EvolutionScenario(Array[EvoOperator](
      Deprec_RemoveEvoOperator(GroundPredicate("open", switcher1)),
      Deprec_AddEvoOperator(GroundPredicate("closed", switcher1)),
      Deprec_RemoveEvoOperator(GroundPredicate("closed", switcher2)),
      Deprec_AddEvoOperator(GroundPredicate("open", switcher2))
    ))
    GroundedAbstractCapability(capname,pre,post,Map("1"-> evo_1),DataInSpecification(ArrayBuffer()),DataOutSpecification(ArrayBuffer()),Map(),"CLOSE_"+name2+"_&_OPEN_"+name2)
  }


  override def termination : TerminationDescription = MaxEmptyIterationTermination(10)
}
