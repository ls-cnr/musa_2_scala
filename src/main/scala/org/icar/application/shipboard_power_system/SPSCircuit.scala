package org.icar.application.shipboard_power_system

import org.icar.pmr_solver.high_level_specification.{AddOperator, Axiom, DomainVariable, EvoOperator, EvolutionGrounding, ExistQuantifier, GroundPredicate, IntegerTerm, NegateCondition, Negation, Predicate, PredicateCondition, RmvOperator, Rule, RuleAntecedent, SystemAction, VariableTerm}


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
case class ConnectionWithFailure(id:Int, source : ElectricNode, dest : ElectricNode)

class SPSCircuit {
	var nodes: List[SimpleNode] = List.empty
	var loads: List[Load] = List.empty
	var generators: List[Generator] = List.empty
	var possible_failures: List[ConnectionWithFailure] = List.empty
	var switcher: List[Switcher] = List.empty

	var switch_dependency: Map[String, String] = Map[String,String]()

	def getLoad(id:Int) : Option[Load] = {
		var ret : Option[Load] = None

		for (l<-loads if l.id==id)
			ret = Some(l)

		ret
	}

	def generate_axioms : Array[Axiom] = {
		var axiom_list : List[Axiom] = List.empty

		for (sw<-switcher) {
			if (sw.source.isInstanceOf[SimpleNode] || sw.source.isInstanceOf[Generator]){
				val ante1 = PredicateCondition( sw.source.up_condition.as_pred )
				val ante2 = PredicateCondition( sw.closed_predicate.as_pred )
				val cons = sw.dest.up_condition.as_pred
				val rule = Rule( cons , RuleAntecedent(List(ante1,ante2)))
				axiom_list = rule :: axiom_list
			}

			if (sw.dest.isInstanceOf[SimpleNode]  || sw.dest.isInstanceOf[Generator]){
				val c_ante1 = PredicateCondition( sw.dest.up_condition.as_pred )
				val c_ante2 = PredicateCondition( sw.closed_predicate.as_pred )
				val c_cons = sw.source.up_condition.as_pred
				val c_rule = Rule( c_cons , RuleAntecedent(List(c_ante1,c_ante2)))

				axiom_list = c_rule :: axiom_list
			}
		}


		axiom_list.toArray
	}

	def generate_actions : Array[SystemAction] = {
		/*
        improvement: EVOLUTION CONSTRAINTS
        for each capability add the corresponsing invariant
        example: close_sw_1 implies that state 'closed(sw1)' holds until the end
		*/

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
				)))
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
				)))
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
				)))
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
				)))
		)


		Array(switch_on_gen,switch_off_gen,close_switcher,open_switcher)
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


