package org.icar.pmr_solver

import org.icar.pmr_solver.HighLevel.{Domain, DomainPredicate, DomainType, DomainVariable, GroundPredicate, IntegerTerm, NumericDomainType, Predicate, PredicateCondition, Rule, RuleAntecedent}
import org.icar.pmr_solver.RETE.RETEBuilder
import org.icar.pmr_solver.Raw.{HL2Raw_Map, RawState}


object Test_BuildRETE_SPS extends App {

	val map = prepare_sps_domain
	println("size of the domain = "+map.inverse.size)

	val wi = RawState(map.state_of_world(List(
		GroundPredicate("on_gen",List(IntegerTerm(1))),

		GroundPredicate("closed_sw",List(IntegerTerm(1))),
		GroundPredicate("closed_sw",List(IntegerTerm(2))),
		GroundPredicate("closed_sw",List(IntegerTerm(4)))
	)))

	val r1_ante1 = PredicateCondition( Predicate("on_gen",List(IntegerTerm(1))) )
	val r1_ante2 = PredicateCondition( Predicate("closed_sw",List(IntegerTerm(1))) )
	val r1_cons = Predicate("up_node",List(IntegerTerm(1)) )
	val rule1 = Rule( r1_cons , RuleAntecedent(List(r1_ante1,r1_ante2)))

	val r2_ante1 = PredicateCondition( Predicate("up_node",List(IntegerTerm(1))) )
	val r2_ante2 = PredicateCondition( Predicate("closed_sw",List(IntegerTerm(2))) )
	val r2_cons = Predicate("up_node",List(IntegerTerm(2)) )
	val rule2 = Rule( r2_cons , RuleAntecedent(List(r2_ante1,r2_ante2)))

	val r3_ante1 = PredicateCondition( Predicate("up_node",List(IntegerTerm(1))) )
	val r3_ante2 = PredicateCondition( Predicate("closed_sw",List(IntegerTerm(3))) )
	val r3_cons = Predicate("up_node",List(IntegerTerm(2)) )
	val rule3 = Rule( r3_cons , RuleAntecedent(List(r3_ante1,r3_ante2)))

	val r4_ante1 = PredicateCondition( Predicate("up_node",List(IntegerTerm(1))) )
	val r4_ante2 = PredicateCondition( Predicate("closed_sw",List(IntegerTerm(4))) )
	val r4_cons = Predicate("up_node",List(IntegerTerm(3)) )
	val rule4 = Rule( r4_cons , RuleAntecedent(List(r4_ante1,r4_ante2)))

	val r5_ante1 = PredicateCondition( Predicate("up_node",List(IntegerTerm(1))) )
	val r5_ante2 = PredicateCondition( Predicate("closed_sw",List(IntegerTerm(5))) )
	val r5_cons = Predicate("up_node",List(IntegerTerm(3)) )
	val rule5 = Rule( r5_cons , RuleAntecedent(List(r5_ante1,r5_ante2)))


	println(wi)
	val rete = RETEBuilder.factory(Array(rule1,rule2,rule3,rule4,rule5),map,wi)
	rete.execute
	println(rete.memory.current)

	println("save")
	val save_memory = rete.memory.copy

	val f1=GroundPredicate("on_gen",List(IntegerTerm(1)))
	val rawf1 = map.direct(f1)

	println("deleting x"+rawf1)
	rete.retract_fact(rawf1)
	rete.execute

	println(rete.memory.current)

	println("load")
	rete.memory = save_memory

	val f2=GroundPredicate("closed_sw",List(IntegerTerm(2)))
	val rawf2 = map.direct(f2)

	println("deleting x"+rawf2)
	rete.retract_fact(rawf2)
	rete.execute
	println(rete.memory.current)








	def prepare_sps_domain : HL2Raw_Map = {
		val dom_types : Array[DomainType] = Array(
			NumericDomainType("gen_id",1,1),
			NumericDomainType("load_id",1,2),
			NumericDomainType("node_id",1,3),
			NumericDomainType("sw_id",1,5),

		)

		val preds : Array[DomainPredicate] = Array(
			DomainPredicate("on_gen",List(
				DomainVariable("ID","gen_id")
			)),
			DomainPredicate("up_load",List(
				DomainVariable("ID","load_id")
			)),
			DomainPredicate("up_node",List(
				DomainVariable("ID","node_id")
			)),
			DomainPredicate("closed_sw",List(
				DomainVariable("ID","sw_id")
			)),
			DomainPredicate("off_gen",List(
				DomainVariable("ID","gen_id")
			)),
			DomainPredicate("down_load",List(
				DomainVariable("ID","load_id")
			)),
			DomainPredicate("down_node",List(
				DomainVariable("ID","node_id")
			)),
			DomainPredicate("open_sw",List(
				DomainVariable("ID","sw_id")
			))
		)

		val domain = Domain(preds,dom_types,Array.empty)
		val map = new HL2Raw_Map(domain)

		map
	}

}
