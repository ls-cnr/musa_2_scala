package org.icar.pmr_solver

import org.icar.fol.{Assumption, AtomTerm, ConstantTerm, GroundLiteral, GroundPredicate, NumeralTerm, StringTerm, VariableTerm, folFormula}

import scala.collection.mutable.ArrayBuffer


/*
 * direct: predicate1 -> var_index (map)
 * inverse: var_index -> predicate1 (arraybuffer)
 */
class PlanningVariableMap(domain: Domain) {
	private var var_counter : Int = 0
	var direct : Map[GroundPredicate,Int] = Map.empty
	var inverse : ArrayBuffer[GroundPredicate] = ArrayBuffer()

	init

	def init = {
		for (p<-domain.predicates) {

			var args = p.args
			combine(p,args,Map.empty)

		}

		def combine(f:DomainPredicate,to_assign:List[DomainPredArguments],assigned:Map[DomainPredArguments,String]):Unit = {
			if (to_assign.isEmpty) {
				register(f,assigned)
			} else {
				val arg : DomainPredArguments = to_assign.head
				for (value <- arg.range){
					combine(f,to_assign.tail,assigned+(arg->value))
				}
			}
		}

		def register(f: DomainPredicate, assigned: Map[DomainPredArguments, String]): Unit = {
			var ground_args : ArrayBuffer[ConstantTerm] = ArrayBuffer()
			for (a <- f.args) {
				val string_value = assigned(a)
				ground_args += StringTerm(string_value)
			}

			val p = GroundPredicate(f.functor,ground_args)

			direct += (p->var_counter)
			inverse += p

			var_counter += 1
		}

	}

	def state_of_world(statements : List[GroundPredicate]):Array[Boolean] = {
		val array : Array[Boolean] = Array.fill(var_counter){false}

		for (s<-statements) {
			val index = direct(s)
			array(index)=true
		}

		array
	}

	def predicate_formula(f:folFormula) : predicate_formula = {
		f match {
			case org.icar.fol.GroundLiteral(p) => xx(direct(p))
			case org.icar.fol.AlwaysTrue() => tt()
			case org.icar.fol.AlwaysFalse() => ff()
			case org.icar.fol.Negation(sf) => negation(predicate_formula(sf))
			case org.icar.fol.Conjunction(terms) =>
				if (terms.size==2)
					conjunction(predicate_formula(terms.head),predicate_formula(terms.tail.head))
				else if (terms.size==1)
					predicate_formula(terms.head)
				else
					conjunction(predicate_formula(terms.head), predicate_formula(org.icar.fol.Conjunction(terms.tail)))
			case org.icar.fol.Disjunction(terms) =>
				if (terms.size==2)
					disjunction(predicate_formula(terms.head),predicate_formula(terms.tail.head))
				else if (terms.size==1)
					predicate_formula(terms.head)
				else
					disjunction(predicate_formula(terms.head), predicate_formula(org.icar.fol.Disjunction(terms.tail)))
			case org.icar.fol.ExistQuantifier(p,vars) =>
				exist_quantifier(p,0,Map.empty)
			case org.icar.fol.UnivQuantifier(p,vars) =>
				foreach_quantifier(p,0,Map.empty)
			case _ => ff()
		}

	}

	def exist_quantifier(p : org.icar.fol.Predicate, pos : Int, assignments : Map[VariableTerm,String]) : predicate_formula = {
		if (pos == p.terms.size) {
			val pred = p.to_ground(assignments)
			xx(direct(pred))

		} else {
			var x_list : List[predicate_formula] = List.empty
			val arg = p.terms(pos)
			arg match {
				case a: VariableTerm =>
					val t : DomainPredArguments = domain.get_predicate_arg_type(p.functional,pos)
					if (assignments.contains(a)) {
						if (t.range.contains(a))
							exist_quantifier(p, pos + 1, assignments)
						else
							ff()
					} else {

						for (value <- t.range){
							x_list = exist_quantifier(p,pos+1,assignments+(a->value)) :: x_list
						}
						combine_in_or(x_list.filter(_!=ff()))
					}
				case a: AtomTerm => exist_quantifier(p,pos+1,assignments)
				case a: NumeralTerm => exist_quantifier(p,pos+1,assignments)
				case a: StringTerm => exist_quantifier(p,pos+1,assignments)

				case _ => ff()
			}
		}
	}

	def foreach_quantifier(p : org.icar.fol.Predicate, pos : Int, assignments : Map[VariableTerm,String]) : predicate_formula = {
		if (pos == p.terms.size) {
			val pred = p.to_ground(assignments)
			xx(direct(pred))

		} else {
			var x_list : List[predicate_formula] = List.empty
			val arg = p.terms(pos)
			arg match {
				case a: VariableTerm =>
					val t : DomainPredArguments = domain.get_predicate_arg_type(p.functional,pos)
					if (assignments.contains(a)) {
						if (t.range.contains(a))
							exist_quantifier(p, pos + 1, assignments)
						else
							tt()
					} else {
						for (value <- t.range) {
							x_list = foreach_quantifier(p, pos + 1, assignments + (a -> value)) :: x_list
						}
						combine_in_and(x_list.filter(_ != tt()))
					}
				case a: AtomTerm => foreach_quantifier(p,pos+1,assignments)
				case a: NumeralTerm => foreach_quantifier(p,pos+1,assignments)
				case a: StringTerm => foreach_quantifier(p,pos+1,assignments)

				case _ => ff()
			}
		}
	}

	def combine_in_or(predicate_formulas: List[predicate_formula]) : predicate_formula = {
		if (predicate_formulas.size==0)
			ff()
		else if (predicate_formulas.size==1)
			predicate_formulas.head
		else
			disjunction(predicate_formulas.head,combine_in_or(predicate_formulas.tail))
	}
	def combine_in_and(predicate_formulas: List[predicate_formula]) : predicate_formula = {
		if (predicate_formulas.size==0)
			ff()
		else if (predicate_formulas.size==1)
			predicate_formulas.head
		else
			conjunction(predicate_formulas.head,combine_in_and(predicate_formulas.tail))
	}

	//def axiom(ass:Assumption)
}
