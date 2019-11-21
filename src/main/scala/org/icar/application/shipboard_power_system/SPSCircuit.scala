package org.icar.application.shipboard_power_system

import org.icar.pmr_solver.high_level_specification.{AddOperator, Axiom, Conjunction, DomainPredicate, DomainType, DomainVariable, EvoOperator, EvolutionGrounding, ExistQuantifier, GroundPredicate, IntegerTerm, NegateCondition, Negation, NumericDomainType, Predicate, PredicateCondition, RmvOperator, Rule, RuleAntecedent, SystemAction, VariableTerm}


abstract class ElectricNode {
	def up_condition : GroundPredicate
	def node_string : String
}
case class SimpleNode(id:Int) extends ElectricNode {
	override def up_condition: GroundPredicate = GroundPredicate("up_node",List(IntegerTerm(id)))
	override def node_string: String = s"N$id"
}
case class Load(id : Int,pow:Float) extends ElectricNode {
	override def up_condition: GroundPredicate = GroundPredicate("up_load",List(IntegerTerm(id)))
	override def node_string: String = s"L$id"
}
case class Generator(id : Int,pow:Float) extends ElectricNode {
	override def up_condition: GroundPredicate = GroundPredicate("on_gen",List(IntegerTerm(id)))
	override def node_string: String = s"G$id"
}

case class Switcher(id: Int, source : ElectricNode, dest : ElectricNode) {
	def closed_predicate : GroundPredicate = GroundPredicate("closed_sw",List(IntegerTerm(id)))
}

case class Selector(id: Int, pos1 : ElectricNode,middle : ElectricNode,pos2 : ElectricNode) {
	def pos1_predicate : GroundPredicate = GroundPredicate("pos1_sel",List(IntegerTerm(id)))
	def pos2_predicate : GroundPredicate = GroundPredicate("pos2_sel",List(IntegerTerm(id)))
}
case class ConnectionWithFailure(id:Int, source : ElectricNode, dest : ElectricNode)





class SPSCircuit {
	var nodes: List[SimpleNode] = List.empty
	var loads: List[Load] = List.empty
	var generators: List[Generator] = List.empty
	var possible_failures: List[ConnectionWithFailure] = List.empty
	var switchers: List[Switcher] = List.empty
	var selectors: List[Selector] = List.empty

	//var switch_dependency: List[Selector] = List.empty

	def getLoad(id:Int) : Option[Load] = {
		var ret : Option[Load] = None

		for (l<-loads if l.id==id)
			ret = Some(l)

		ret
	}

	def generate_domainn_types : Array[DomainType] = {
		Array(
			NumericDomainType("gen_id",1,generators.size),
			NumericDomainType("load_id",1,loads.size),
			NumericDomainType("node_id",1,nodes.size),
			NumericDomainType("sel_id",1,selectors.size),
			NumericDomainType("sw_id",1,switchers.size)
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

		for (sel<-selectors) {
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

		axiom_list.toArray
	}

	def generate_actions : Array[SystemAction] = {
		val switch_on_gen = SystemAction(
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

			invariants = List(Predicate("on_gen", List(VariableTerm("ID"))))
		)

		val switch_off_gen = SystemAction(
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

			invariants = List(Negation(Predicate("on_gen", List(VariableTerm("ID")))))
		)

		val close_switcher = SystemAction(
			id = "close_switcher",
			params = List(DomainVariable("ID","sw_id")),

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

			invariants = List(Predicate("closed_sw", List(VariableTerm("ID"))))
		)

		val open_switcher = SystemAction(
			id = "open_switcher",
			params = List(DomainVariable("ID","sw_id")),

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

			invariants = List(Negation(Predicate("closed_sw", List(VariableTerm("ID")))))
		)

		val selector_pos1 = SystemAction(
			id = "selector_pos1",
			params = List(DomainVariable("ID","sel_id")),

			pre = Negation(Predicate("pos1_sel", List(VariableTerm("ID")))),

			post = Predicate("pos1_sel", List(VariableTerm("ID"))),

			effects = Array(
				EvolutionGrounding("base",Array[EvoOperator](
					AddOperator(Predicate("pos1_sel", List( VariableTerm("ID")))),
					RmvOperator(Predicate("pos2_sel", List( VariableTerm("ID"))))
				))),

			invariants = List(Predicate("pos1_sel", List(VariableTerm("ID"))))
		)

		val selector_pos2 = SystemAction(
			id = "selector_pos2",
			params = List(DomainVariable("ID","sel_id")),

			pre = Negation(Predicate("pos2_sel", List(VariableTerm("ID")))),

			post = Predicate("pos2_sel", List(VariableTerm("ID"))),

			effects = Array(
				EvolutionGrounding("base",Array[EvoOperator](
					AddOperator(Predicate("pos2_sel", List( VariableTerm("ID")))),
					RmvOperator(Predicate("pos1_sel", List( VariableTerm("ID"))))
				))),

			invariants = List(Predicate("pos2_sel", List(VariableTerm("ID"))))
		)

		Array(switch_on_gen,switch_off_gen,close_switcher,open_switcher,selector_pos1,selector_pos2)
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


