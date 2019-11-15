package org.icar.pmr_solver

import org.icar.pmr_solver.HighLevel.{AtomTerm, Domain, DomainPredicate, DomainType, DomainVariable, EnumerativeDomainType, GroundPredicate, IntegerTerm, NumericDomainType, Predicate, PredicateCondition, Rule, RuleAntecedent, VariableTerm}
import org.icar.pmr_solver.RETE.RETEBuilder
import org.icar.pmr_solver.Raw.{HL2Raw_Map, RawState}

object Test_BuildRETE extends App {

	val map = prepare_domain
	println("size of the domain = "+map.inverse.size)
	val wi = RawState(map.state_of_world(List()))

	val cons = Predicate("cc",List(VariableTerm("a"),VariableTerm("s")))
	val ante1 = PredicateCondition( Predicate("f",List(VariableTerm("a"),VariableTerm("b"),VariableTerm("c"))) )
	val ante2 = PredicateCondition( Predicate("g",List(VariableTerm("z"),VariableTerm("s"),VariableTerm("a"))) )
	val ante3 = PredicateCondition( Predicate("h",List(VariableTerm("b"),VariableTerm("s"))) )
	val rule = Rule( cons , RuleAntecedent(List(ante1,ante2,ante3)))

	val rete = RETEBuilder.factory(Array(rule),map,wi)


	val p1_f = GroundPredicate("f",List(IntegerTerm(0),IntegerTerm(1),AtomTerm("issue_list")))
	val p2_g = GroundPredicate("g",List(IntegerTerm(3),AtomTerm("paper"),IntegerTerm(0)))
	val p3_h = GroundPredicate("h",List(IntegerTerm(1),AtomTerm("paper")))

	val r1_f = map.direct(p1_f)
	val r2_g = map.direct(p2_g)
	val r3_h = map.direct(p3_h)

	rete.add_fact(r1_f)
	rete.add_fact(r2_g)
	rete.add_fact(r3_h)



	def prepare_domain : HL2Raw_Map = {
		val dom_types : Array[DomainType] = Array(
			EnumerativeDomainType("doc_type",Array("issue_list","paper","tech_rep")),
			NumericDomainType("doc_num",0,3)
		)

		val preds : Array[DomainPredicate] = Array(
			DomainPredicate("f",List(
				DomainVariable("N1","doc_num"),
				DomainVariable("N2","doc_num"),
				DomainVariable("TYPE","doc_type")
			)),
			DomainPredicate("g",List(
				DomainVariable("N1","doc_num"),
				DomainVariable("TYPE","doc_type"),
				DomainVariable("N2","doc_num")
			)),
			DomainPredicate("h",List(
				DomainVariable("N1","doc_num"),
				DomainVariable("TYPE","doc_type")
			)),
			DomainPredicate("cc",List(
				DomainVariable("N1","doc_num"),
				DomainVariable("TYPE","doc_type")
			))
		)

		val domain = Domain(preds,dom_types,Array.empty)
		val map = new HL2Raw_Map(domain)

		map
	}

}
