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

	/* alpha */
	val aval_pred = Predicate("document", List(VariableTerm("TYPE"), AtomTerm("available")))
	val available_match_list = map.all_matching_vars(aval_pred)

	val rece_pred = Predicate("document", List(VariableTerm("TYPE"), AtomTerm("received")))
	val received_match_list = map.all_matching_vars(rece_pred)


	val alpha_available : AlphaNode = new AlphaNode(map)
	for (v<-available_match_list)
		alpha_available.setToken(v.index,false)

	val alpha_received : AlphaNode = new AlphaNode(map)
	for (v<-received_match_list)
		alpha_received.setToken(v.index,false)

	/* beta */
	val beta_ava_rece = new BetaTwoInputNode(alpha_available,0,alpha_received,0)


	/* rete */
	val rete = new RETE

	val r1 = new PNode(1,"document", List(Match(0), Fix(AtomTerm("ready"))), map, rete)
	rete.root.subnodes = List(alpha_available,alpha_received)
	alpha_available.subnodes = List(beta_ava_rece)
	alpha_received.subnodes = List(beta_ava_rece)
	beta_ava_rece.subnodes = List(r1)


	println("new fact: 18")
	rete.add_fact(18)
	println("new fact: 17")
	rete.add_fact(17)
	println("remove fact: 18")
	rete.retract_fact(18)
	println("again fact: 18")
	rete.add_fact(18)


}
