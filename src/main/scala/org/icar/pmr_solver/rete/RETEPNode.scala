package org.icar.pmr_solver.rete

import org.icar.pmr_solver.high_level_specification.{ConstantTerm, GroundPredicate}
import org.icar.pmr_solver.symbolic_level.{HL2Raw_Map, RawVar}

class PMemory() {
	var inference_list : List[Inference] = List.empty

	def copy : PMemory = {
		val pmemory = new PMemory
		pmemory.inference_list = for (i<-inference_list) yield i
		pmemory
	}
}

class PNode(rete:RETE, ID:Int, rule_priority: Int, functor:String, args:List[InferenceTerms], domain:HL2Raw_Map) extends RETENode {

	override def add_token(ass: MatchingToken, source: RETENode): Unit = {
		var terms : List[ConstantTerm] = List.empty
		for (a<-args) {
			a match {
				case Match(index) =>
					val term = ass.term_list(index)
					terms = term :: terms

				case Fix(term) => terms = term :: terms

			}
		}

		val p = GroundPredicate( functor, terms.reverse )
		val inference = RawVar(domain.direct(p))
		rete.memory.p_memory(ID).inference_list = Inference(inference,ass.dependency) :: rete.memory.p_memory(ID).inference_list

		//println(s"** deduced $p => $inference")
		rete.insert_deduction(inference,rule_priority)
	}

	override def retract_fact(index: Int, source: RETENode): Unit = {
		var new_inference_list : List[Inference] = List.empty

		for (inf<-rete.memory.p_memory(ID).inference_list) {
			if (inf.dependency.contains(index)) {
				val variable = inf.v
				val predicate = domain.inverse(variable.index)

				//println(s"** retract $predicate => $variable")
				rete.remove_deduction(variable)
			} else {
				new_inference_list = inf :: new_inference_list
			}
		}

		rete.memory.p_memory(ID).inference_list = new_inference_list
	}

}
