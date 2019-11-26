package org.icar.application.shipboard_power_system

import org.icar.pmr_solver.high_level_specification._


class SPSCircuit {
	var nodes: List[SimpleNode] = List.empty
	var loads: List[Load] = List.empty
	var generators: List[Generator] = List.empty
	var possible_failures: List[ConnectionWithFailure] = List.empty
	var switchers: List[Switcher] = List.empty
	var twowayselectors: List[TwoWaySelector] = List.empty
	var mutex_switchers : List[(Switcher,Switcher)] = List.empty

	def getLoadByID(id:Int) : Option[Load] = {
		var ret : Option[Load] = None
		for (l<-loads if l.id==id) ret = Some(l)
		ret
	}
	def getLoadByName(name:String) : Option[Load] = {
		var ret : Option[Load] = None
		for (l<-loads if l.name==name) ret = Some(l)
		ret
	}

	def is_mutex(switcher: Switcher) : Boolean = {
		var f=false
		for (m<-mutex_switchers) if (m._1==switcher || m._2==switcher) f=true
		f
	}

	def generate_domain_types : Array[DomainType] = {
		val gen_enum = for (g<-generators) yield g.name
		val load_enum = for (l<-loads) yield l.name
		val sw_enum = for (sw<-switchers) yield sw.name
		val sel_enum = for (s<-twowayselectors) yield s.name

		val sw_range: List[String] = for (sw<-switchers if !is_mutex(sw)) yield sw.name
		Array(// se non ci sono twowayselectors rimuovere l'elemento
			StringEnum_DomainType("gen_id",gen_enum.toArray),
			StringEnum_DomainType("load_id",load_enum.toArray),
			StringEnum_DomainType("sw_id",sw_enum.toArray),
			StringEnum_DomainType("sel_id",sel_enum.toArray),
			IntegerRange_DomainType("node_id",1,nodes.size),
			IntegerRange_DomainType("failure_id",1,possible_failures.size),
			StringEnum_DomainType("free_sw_id",sw_range.toArray)
		)
	}
	def generate_predicates : Array[DomainPredicate] = {
		Array(
			DomainPredicate("on_gen",List(
				DomainVariable("ID","gen_id")
			)),
			DomainPredicate("up_load",List(
				DomainVariable("ID","load_id")
			)),
			DomainPredicate("up_node",List(
				DomainVariable("ID","node_id")
			)),
			DomainPredicate("failure",List(
				DomainVariable("ID","failure_id")
			)),
			DomainPredicate("pos1_sel",List(
				DomainVariable("ID","sel_id")
			)),
			DomainPredicate("pos2_sel",List(
				DomainVariable("ID","sel_id")
			)),
			DomainPredicate("closed_sw",List(
				DomainVariable("ID","sw_id")
			))
		)
	}
	def generate_axioms : Array[Axiom] = {
		var axiom_list : List[Axiom] = List.empty

		for (sw<-switchers) {
			// source ^ closed => dest
			if (!sw.dest.isInstanceOf[Generator]){
				val ante1 = PredicateCondition( sw.source.up_condition.as_pred )
				val ante2 = PredicateCondition( sw.closed_predicate.as_pred )
				val cons = sw.dest.up_condition.as_pred
				val rule = Rule( cons , RuleAntecedent(List(ante1,ante2)))
				axiom_list = rule :: axiom_list
			}

			// dest ^ closed => source
			if (!sw.source.isInstanceOf[Generator]){
				val c_ante1 = PredicateCondition( sw.dest.up_condition.as_pred )
				val c_ante2 = PredicateCondition( sw.closed_predicate.as_pred )
				val c_cons = sw.source.up_condition.as_pred
				val c_rule = Rule( c_cons , RuleAntecedent(List(c_ante1,c_ante2)))

				axiom_list = c_rule :: axiom_list
			}
		}

		for (sel<-twowayselectors) {
			val pos1_is_up = sel.pos1.up_condition.as_pred
			val pos2_is_up = sel.pos2.up_condition.as_pred
			val central_is_up = sel.middle.up_condition.as_pred
			val sel_is_pos1 = sel.pos1_predicate.as_pred
			val sel_is_pos2 = sel.pos2_predicate.as_pred

			// pos1_node ^ pos1 => central_node
			if (!sel.middle.isInstanceOf[Generator]) {
				val rule = Rule( central_is_up , RuleAntecedent(List(PredicateCondition(pos1_is_up),PredicateCondition(sel_is_pos1))))
				axiom_list = rule :: axiom_list
			}

			// central_node ^ pos1 => pos1_node
			if (!sel.pos1.isInstanceOf[Generator]) {
				val rule = Rule( pos1_is_up , RuleAntecedent(List(PredicateCondition(central_is_up),PredicateCondition(sel_is_pos1))))
				axiom_list = rule :: axiom_list
			}

			// pos2_node ^ pos2 => central_node
			if (!sel.middle.isInstanceOf[Generator]) {
				val rule = Rule( central_is_up , RuleAntecedent(List(PredicateCondition(pos2_is_up),PredicateCondition(sel_is_pos2))))
				axiom_list = rule :: axiom_list
			}

			// central_node ^ pos2 => pos2_node
			if (!sel.pos2.isInstanceOf[Generator]) {
				val rule = Rule( pos2_is_up , RuleAntecedent(List(PredicateCondition(central_is_up),PredicateCondition(sel_is_pos2))))
				axiom_list = rule :: axiom_list
			}
		}

		for (fail<-possible_failures) {
			// source ^ !failure => dest
			if (!fail.dest.isInstanceOf[Generator]){
				val ante1 = PredicateCondition( fail.source.up_condition.as_pred )
				val ante2 = NegateCondition( fail.fired_predicate.as_pred )
				val cons = fail.dest.up_condition.as_pred
				val rule = Rule( cons , RuleAntecedent(List(ante1,ante2)))
				axiom_list = rule :: axiom_list
			}

			// dest ^ closed => source
			if (!fail.source.isInstanceOf[Generator]){
				val c_ante1 = PredicateCondition( fail.dest.up_condition.as_pred )
				val c_ante2 = NegateCondition( fail.fired_predicate.as_pred )
				val c_cons = fail.source.up_condition.as_pred
				val c_rule = Rule( c_cons , RuleAntecedent(List(c_ante1,c_ante2)))

				axiom_list = c_rule :: axiom_list
			}
		}

		axiom_list.toArray
	}
	def generate_actions : Array[AbstractCapability] = {
		val switch_on_gen = AbstractCapability(
			id = "switch_on_gen",
			params = List(DomainVariable("ID","gen_id")),

			pre = Negation(ExistQuantifier(
				List(VariableTerm("ID")),
				Predicate("on_gen", List(VariableTerm("ID")))
			)),

			post = ExistQuantifier(
				List(VariableTerm("ID")),
				Predicate("on_gen", List(VariableTerm("ID")))
			),

			effects = Array(
				EvolutionGrounding("base",Array[EvoOperator](
					AddOperator(Predicate("on_gen", List( VariableTerm("ID"))))
				))),

			future = List(Predicate("on_gen", List(VariableTerm("ID"))))
		)

		val switch_off_gen = AbstractCapability(
			id = "switch_off_gen",
			params = List(DomainVariable("ID","gen_id")),

			pre = ExistQuantifier(
				List(VariableTerm("ID")),
				Predicate("on_gen", List(VariableTerm("ID")))
			),

			post = Negation(ExistQuantifier(
				List(VariableTerm("ID")),
				Predicate("on_gen", List(VariableTerm("ID")))
			)),

			effects = Array(
				EvolutionGrounding("base",Array[EvoOperator](
					RmvOperator(Predicate("on_gen", List( VariableTerm("ID"))))
				))),

			future = List(Negation(Predicate("on_gen", List(VariableTerm("ID")))))
		)

		val close_switcher = AbstractCapability(
			id = "close_switcher",
			params = List(DomainVariable("ID","free_sw_id")),

			pre = Negation(ExistQuantifier(
				List(VariableTerm("ID")),
				Predicate("closed_sw", List(VariableTerm("ID")))
			)),

			post = ExistQuantifier(
				List(VariableTerm("ID")),
				Predicate("closed_sw", List(VariableTerm("ID")))
			),

			effects = Array(
				EvolutionGrounding("base",Array[EvoOperator](
					AddOperator(Predicate("closed_sw", List( VariableTerm("ID"))))
				))),

			future = List(Predicate("closed_sw", List(VariableTerm("ID"))))
		)

		val open_switcher = AbstractCapability(
			id = "open_switcher",
			params = List(DomainVariable("ID","free_sw_id")),

			pre = ExistQuantifier(
				List(VariableTerm("ID")),
				Predicate("closed_sw", List(VariableTerm("ID")))
			),

			post = Negation(ExistQuantifier(
				List(VariableTerm("ID")),
				Predicate("closed_sw", List(VariableTerm("ID")))
			)),

			effects = Array(
				EvolutionGrounding("base",Array[EvoOperator](
					RmvOperator(Predicate("closed_sw", List( VariableTerm("ID"))))
				))),

			future = List(Negation(Predicate("closed_sw", List(VariableTerm("ID")))))
		)

		val selector_pos1 = AbstractCapability(
			id = "selector_pos1",
			params = List(DomainVariable("ID","sel_id")),

			pre = Negation(Predicate("pos1_sel", List(VariableTerm("ID")))),

			post = Predicate("pos1_sel", List(VariableTerm("ID"))),

			effects = Array(
				EvolutionGrounding("base",Array[EvoOperator](
					AddOperator(Predicate("pos1_sel", List( VariableTerm("ID")))),
					RmvOperator(Predicate("pos2_sel", List( VariableTerm("ID"))))
				))),

			future = List(Predicate("pos1_sel", List(VariableTerm("ID"))))
		)

		val selector_pos2 = AbstractCapability(
			id = "selector_pos2",
			params = List(DomainVariable("ID","sel_id")),

			pre = Negation(Predicate("pos2_sel", List(VariableTerm("ID")))),

			post = Predicate("pos2_sel", List(VariableTerm("ID"))),

			effects = Array(
				EvolutionGrounding("base",Array[EvoOperator](
					AddOperator(Predicate("pos2_sel", List( VariableTerm("ID")))),
					RmvOperator(Predicate("pos1_sel", List( VariableTerm("ID"))))
				))),

			future = List(Predicate("pos2_sel", List(VariableTerm("ID"))))
		)

		var list_of_actions : List[AbstractCapability] = List.empty
		var mutex_counter = 1
		for (m<-mutex_switchers) {
			val sw1=m._1.closed_predicate.as_pred
			val sw2=m._2.closed_predicate.as_pred

			val mutex_action_pos1 = AbstractCapability(
				id = "mutex_"+mutex_counter+"_pos1",
				params = List(),

				pre = Conjunction(List(Negation(sw1),sw2)),

				post = Conjunction(List(sw1,Negation(sw2))),

				effects = Array(
					EvolutionGrounding("base",Array[EvoOperator](
						AddOperator(sw1),
						RmvOperator(sw2)
					))),

				future = List(sw1,Negation(sw2))
			)

			val mutex_action_pos2 = AbstractCapability(
				id = "mutex_"+mutex_counter+"_pos2",
				params = List(),

				pre = Conjunction(List(sw1,Negation(sw2))),

				post = Conjunction(List(sw2,Negation(sw1))),

				effects = Array(
					EvolutionGrounding("base",Array[EvoOperator](
						AddOperator(sw2),
						RmvOperator(sw1)
					))),

				future = List(sw2,Negation(sw1))
			)

			list_of_actions = mutex_action_pos1 :: mutex_action_pos2 :: list_of_actions
			mutex_counter += 1
		}

		list_of_actions = List(switch_on_gen,switch_off_gen,close_switcher,open_switcher,selector_pos1,selector_pos2):::list_of_actions

		list_of_actions.toArray
	}
	def generate_goal(mission: SPSMission) : HL_LTLFormula = {
		var formulas : List[HL_LTLFormula] = List.empty
		for (vital<-mission.vitals){
			val l = getLoadByName(vital)
			if (l.isDefined) {
				val up_pred = l.get.up_condition
				formulas = up_pred :: formulas
			}
		}
		Finally(Conjunction(formulas))
	}

	def print_for_graphviz_with_field(f:ForceFieldLayer) : Unit = {
		println("digraph Circuit {")

		//nodes.foreach( n=> println(s"${n.node_string} [label=\"(${f.map(n)})\"]; ") )
		nodes.foreach( n=> println(s"${n.node_string}; ") )
		loads.foreach( n=> println(s"${n.node_string} [shape=invtriangle,color=black]; ") )
		generators.foreach( n=> println(s"${n.node_string} [shape=box,color=red];") )

		//possible_failures.foreach( pf => println(s"${pf.source.node_string} -> ${pf.dest.node_string} [label=\"failure=${pf.id}\"];"))
		//switcher.foreach( s=> println(s"${s.source.node_string} -> ${s.dest.node_string} [label=\"switch=${s.id}\"];"))

		println("}")
	}
	def print_for_graphviz() : Unit = {
		println("digraph Circuit {")

		nodes.foreach( n=> println(s"${n.node_string}; ") )
		loads.foreach( n=> println(s"${n.node_string} [shape=invtriangle,color=black]; ") )
		generators.foreach( n=> println(s"${n.node_string} [shape=box,color=red];") )

		//possible_failures.foreach( f => println(s"${f.source.node_string} -> ${f.dest.node_string} [label=\"failure=${f.id}\"];"))
		//switcher.foreach( s=> println(s"${s.source.node_string} -> ${s.dest.node_string} [label=\"switch=${s.id}\"];"))

		println("}")
	}
}


object SPSCircuit {

	def build_from_file(file:String) : SPSCircuit = {
		val circuit = new SPSCircuit

		var node_counter = 1
		var load_counter = 1
		var conn_counter = 1
		var gen_counter = 1
		var switcher_counter = 1
		var failure_counter = 1
		var node_to_electricnode : Map[Int,ElectricNode] = Map.empty
		var name_to_switcher : Map[String,Switcher] = Map.empty

		def add_load(name:String, node:Int, pow:Float): Unit = {
			val load = Load(load_counter,name,pow)
			load_counter += 1
			node_to_electricnode += (node->load)
			circuit.loads = load :: circuit.loads
		}
		def add_generator(name:String, node:Int, pow:Float): Unit = {
			val gen = Generator(gen_counter,name,pow)
			gen_counter += 1
			node_to_electricnode += (node -> gen)
			circuit.generators = gen :: circuit.generators
		}
		def add_connection(node1: Int, node2: Int): Unit = {
			val src_is_new = !node_to_electricnode.contains(node1)
			val dst_is_new = !node_to_electricnode.contains(node2)

			if (!src_is_new && !dst_is_new) {
				val src_node = node_to_electricnode(node1)
				val dst_node = node_to_electricnode(node2)
				circuit.possible_failures = ConnectionWithFailure(conn_counter,src_node,dst_node) :: circuit.possible_failures
				conn_counter += 1
			} else if (!src_is_new && dst_is_new ) {
				val src_node = node_to_electricnode(node1)
				node_to_electricnode += (node2 -> src_node)
			} else if (src_is_new && !dst_is_new ) {
				val dst_node = node_to_electricnode(node2)
				node_to_electricnode += (node1 -> dst_node)
			} else {
				circuit.nodes = SimpleNode(node_counter) :: circuit.nodes
				node_to_electricnode += (node1 -> SimpleNode(node_counter))
				node_to_electricnode += (node2 -> SimpleNode(node_counter))
				node_counter += 1
			}
		}
		def add_switcher(name:String, node1: Int, node2: Int): Unit = {
			val src = node_to_electricnode(node1)
			val dst = node_to_electricnode(node2)
			if (!is_failure(name)) {
				val switcher = Switcher(switcher_counter,name,src,dst)
				switcher_counter += 1
				name_to_switcher += (name->switcher)
				circuit.switchers = switcher :: circuit.switchers
			} else {
				val failure = ConnectionWithFailure(failure_counter,src,dst)
				failure_counter += 1
				circuit.possible_failures = failure :: circuit.possible_failures
			}
		}
		def is_failure(str: String):Boolean = {
			var r=false
			if (str.startsWith("switch")) {
				val real_name = str.substring(6)
				if (real_name.startsWith("f"))
					r=true
			}
			r
		}
		def add_mutex(sw1: String, sw2: String): Unit = {
			val switcher1 = name_to_switcher(sw1)
			val switcher2 = name_to_switcher(sw2)
			circuit.mutex_switchers = (switcher1,switcher2) :: circuit.mutex_switchers
		}

		val spec : CircuitSpec = SimulinkParser.load_circuit_spec(file)
		spec.loads.foreach( c=>add_load(c._1,c._2,c._3) )
		spec.gens.foreach( c=>add_generator(c._1,c._2,c._3) )
		spec.connections.foreach(c=>add_connection(c._1,c._2) )
		spec.switchers.foreach(c=>add_switcher(c._1,c._2,c._3) )
		spec.mutex.foreach( m=>add_mutex(m._1,m._2))

		circuit
	}

	def sample_circuit : SPSCircuit = {
		val circuit = new SPSCircuit

		circuit.generators = List( Generator(1,"mg1",100) )
		circuit.loads = List( Load(1,"load1",50),Load(2,"load2",50))
		circuit.nodes = List( SimpleNode(1),SimpleNode(2),SimpleNode(3),SimpleNode(4),SimpleNode(5) )
		circuit.possible_failures = List( ConnectionWithFailure(1,SimpleNode(1),SimpleNode(5)) )
		circuit.switchers = List(
			Switcher(1,"sw1",SimpleNode(3),Load(1,"load1",50)),
			Switcher(2,"sw2",SimpleNode(4),Load(2,"load2",50)),
			Switcher(3,"sw3",Generator(1,"mg1",100),SimpleNode(1)),
			Switcher(4,"sw4",Generator(1,"mg1",100),SimpleNode(2))
		)
		circuit.twowayselectors = List(
			//TwoWaySelector(1,SimpleNode(1),Generator(1,100),SimpleNode(2)),
			TwoWaySelector(1,"sel1",SimpleNode(1),SimpleNode(3),SimpleNode(2)),
			TwoWaySelector(2,"sel2",SimpleNode(5),SimpleNode(4),SimpleNode(2))
		)
		circuit.mutex_switchers = List(
			(Switcher(3,"sw3",Generator(1,"mg1",100),SimpleNode(1)), Switcher(4,"sw4",Generator(1,"mg1",100),SimpleNode(2)))
		)
		circuit
	}
	def sample_circuit_mission = SPSMission(List("load1","load2"),List.empty,List.empty)
	def sample_circuit_initial = StateOfWorld(List(
		GroundPredicate("on_gen",List(AtomTerm("mg1"))),
		GroundPredicate("closed_sw",List(AtomTerm("sw3"))),
		GroundPredicate("pos1_sel",List(AtomTerm("sel1"))),
		GroundPredicate("pos1_sel",List(AtomTerm("sel2"))),

		GroundPredicate("failure",List(IntegerTerm(1))),

	))

	def circuit_3_mission = {
		val vitals = List("load2","load6","load12","load16","load22","load3","load7","load9")
		val semivitals =List("load13","load17","load19","load23","load1","load4","load5")
		val nonvitals =List("load8","load11","load14","load15","load18","load21","load24")
		SPSMission(vitals,semivitals,nonvitals)
	}
	def circuit_3_initial = {
		val on_generators = List("mg1","mg2")
		val closed_switchers = List("switchsws1","switchsws2","switchsws3","switchsws4","switchsws5","switchsws6","switchsws7")
		val failures = List()//List(1,2,3)

		val on_gen_preds = for(i<-on_generators) yield GroundPredicate("on_gen",List(AtomTerm(i)))
		val closed_switcher_preds = for(i<-closed_switchers) yield GroundPredicate("closed_sw",List(AtomTerm(i)))
		val failure_preds = for (i<-failures) yield GroundPredicate("failure",List(IntegerTerm(i)))

		StateOfWorld(on_gen_preds:::closed_switcher_preds:::failure_preds)
	}
}