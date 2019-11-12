package org.icar.pmr_solver

object ReteBuilder {

	def factory(axioms : Array[Axiom]) : RootNode = {
		val root = new RootNode
		var alpha_list : List[AlphaNode] = List.empty
		var beta_list : List[BetaJoinNode] = List.empty

		for (rule<-axioms) {
			rule match {
				case Rule(rhs,lhs) =>
					for (t<-lhs.terms) {
						t match {
							case PredicateCondition(p) => /* alpha node */

							case NegateCondition(p) => /* negated alpha node */

							case EqualTestCondition() => /* one-input beta node */

							case LessTestCondition() => /* one-input beta node */

							case GreaterTestCondition() => /* one-input beta node */

						}

					}

				case _ =>

			}
		}




		root
	}
}