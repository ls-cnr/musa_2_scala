package org.icar.pmr_solver

object Test_RETE extends App {

	def prepare_domain : HL2Raw_Map = {
		val dom_types : Array[DomainType] = Array(
			EnumerativeDomainType("doc_type",Array("issue_list","paper","tech_rep")),
			EnumerativeDomainType("doc_state",Array("ready","available","received","registered","worked","accepted","rejected","to_revise"))
		)

		val preds : Array[DomainPredicate] = Array(
			DomainPredicate("document",List(
				DomainVariable("TYPE","doc_type"),
				DomainVariable("STATE","doc_state")
			))
		)

		val domain = Domain(preds,dom_types,Array.empty,_=>0)
		val map = new HL2Raw_Map(domain)

		map
	}

	val map = prepare_domain
	val wi = RawState(map.state_of_world(List(

	)))


	/* alpha */
	val aval_pred = Predicate("document", List(VariableTerm("TYPE"), AtomTerm("available")))
	val alpha_available : AlphaNode = new AlphaNode(map,aval_pred,wi)

	val rece_pred = Predicate("document", List(VariableTerm("TYPE"), AtomTerm("received")))
	val alpha_received : AlphaNode = new AlphaNode(map,rece_pred,wi)


	/*
	val rece_pred = Predicate("document", List(VariableTerm("TYPE"), AtomTerm("received")))
	val alpha_received : NegatedAlphaNode = new NegatedAlphaNode(map,rece_pred,wi)
	*/

	/* beta */
	def equalcondition(con1: ConstantTerm)(con2 : ConstantTerm): Boolean = {con1==con2}
	val beta_condition = new BetaOneInputNode(equalcondition(AtomTerm("tech_rep")),0)

	val beta_ava_rece = new BetaTwoInputNode(beta_condition,0,alpha_received,0)


	/* rete */
	val rete = new RETE

	val r1 = new PNode(1,"document", List(Match(0), Fix(AtomTerm("ready"))), map, rete)
	rete.root.subnodes = List(alpha_available,alpha_received)
	alpha_available.subnodes = List(beta_condition)
	beta_condition.subnodes = List(beta_ava_rece)

	alpha_received.subnodes = List(beta_ava_rece)

	beta_ava_rece.subnodes = List(r1)

	rete.start

	println("new fact: 18")
	rete.add_fact(18)
	println("new fact: 17")
	rete.add_fact(17)
	println("remove fact: 18")
	rete.retract_fact(18)
	println("again fact: 18")
	rete.add_fact(18)


}
