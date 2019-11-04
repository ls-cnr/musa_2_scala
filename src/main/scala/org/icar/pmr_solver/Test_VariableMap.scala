package org.icar.pmr_solver

import org.icar.fol.{AtomTerm, GroundLiteral, GroundPredicate, Literal, StringTerm, VariableTerm}

import scala.collection.mutable.ArrayBuffer

object Test_VariableMap extends App {
	def qos(n:RawState):Float=0

	val available_args : List[DomainPredArguments] = List(
		DomainVariable("X",NumericDomainType(0,5)),
		DomainVariable("Y",EnumerativeDomainType(Array("issue","thesis","reading"))),
		DomainConstant("luca")
	)

	val preds : Array[DomainPredicate] = Array(
		DomainPredicate("available",available_args),
		DomainPredicate("document",List(DomainVariable("Y",EnumerativeDomainType(Array("ready","in_preparation","worked")))))
	)

	val d = Domain(preds,Array.empty,qos)

	val map = new PlanningVariableMap(d)

	println("Size of predicates: "+map.inverse.size)

	println(map.inverse)
	println(map.direct)

	val w = map.state_of_world(List(
		GroundPredicate("available", StringTerm("0"),StringTerm("issue"),StringTerm("luca")),
		GroundPredicate("available", StringTerm("1"),StringTerm("reading"),StringTerm("luca")),
		GroundPredicate("document", StringTerm("ready"))
	))

	print("[")
	for (b<-w)
		if(b) print(1) else print(0)
	println("]")

	val formula = org.icar.fol.Disjunction (
		org.icar.fol.Conjunction(
			GroundLiteral(org.icar.fol.GroundPredicate("document", StringTerm("ready"))),
			GroundLiteral(org.icar.fol.GroundPredicate("available", StringTerm("1"),StringTerm("reading"),StringTerm("luca")))
		),
		org.icar.fol.Conjunction(
			GroundLiteral(org.icar.fol.GroundPredicate("document", StringTerm("worked"))),
			org.icar.fol.Negation(GroundLiteral(org.icar.fol.GroundPredicate("available", StringTerm("1"),StringTerm("reading"),StringTerm("luca"))))
		)
	)

	//val exist_formula = org.icar.fol.ExistQuantifier(org.icar.fol.Predicate("available",VariableTerm("X"),StringTerm("issue"),StringTerm("luca")),ArrayBuffer(VariableTerm("X")))
	//val exist_formula = org.icar.fol.ExistQuantifier(org.icar.fol.Predicate("available",StringTerm("0"),VariableTerm("X"),StringTerm("luca")),ArrayBuffer(VariableTerm("X")))
	//val exist_formula = org.icar.fol.ExistQuantifier(org.icar.fol.Predicate("available",VariableTerm("X"),VariableTerm("Y"),StringTerm("luca")),ArrayBuffer(VariableTerm("X")))
	val exist_formula = org.icar.fol.ExistQuantifier(org.icar.fol.Predicate("available",VariableTerm("X"),VariableTerm("X"),StringTerm("luca")),ArrayBuffer(VariableTerm("X")))

	val foreach_formula = org.icar.fol.UnivQuantifier(org.icar.fol.Predicate("available",StringTerm("0"),VariableTerm("X"),StringTerm("luca")),ArrayBuffer(VariableTerm("X")))

	val f = map.predicate_formula(formula)
	println(f)

	val e = map.predicate_formula(exist_formula)
	println(e)

	val e2 = map.predicate_formula(foreach_formula)
	println(e2)

}
