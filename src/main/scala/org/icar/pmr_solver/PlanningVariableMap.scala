package org.icar.pmr_solver

import org.icar.fol.{Assumption, AtomTerm, ConstantTerm, GroundLiteral, GroundPredicate, HighLevel_PredicateFormula, NumeralTerm, StringTerm, Term, TweetyFormula, VariableTerm}
import org.icar.musa.context.{AddOperator, Deprec_AddEvoOperator, Deprec_RemoveAllEvoOperator, Deprec_RemoveEvoOperator, EvoOperator, RmvOperator}
import org.icar.musa.main_entity.EvolutionScenario

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

		def combine(f:DomainPredicate,to_assign:List[DomainPredArguments],assigned:Map[DomainPredArguments,ConstantTerm]):Unit = {
			if (to_assign.isEmpty) {
				register(f,assigned)
			} else {
				val arg : DomainPredArguments = to_assign.head
				for (value <- arg.range(domain.types)){
					combine(f,to_assign.tail,assigned+(arg->value))
				}
			}
		}

		def register(f: DomainPredicate, assigned: Map[DomainPredArguments, ConstantTerm]): Unit = {
			var ground_args : ArrayBuffer[ConstantTerm] = ArrayBuffer()
			for (a <- f.args)
				ground_args += assigned(a)

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

	def predicate_formula(f:HighLevel_PredicateFormula) : RawPredicate = {
		def exist_quantifier(p : org.icar.fol.Predicate, pos : Int, assignments : Map[VariableTerm,ConstantTerm]) : RawPredicate = {
			if (pos == p.terms.size) {
				val pred = p.to_ground(assignments)
				RawVar(direct(pred))

			} else {
				var x_list : List[RawPredicate] = List.empty
				val arg = p.terms(pos)
				arg match {
					case a: VariableTerm =>
						val t : DomainPredArguments = domain.get_predicate_arg_type(p.functional,pos)
						if (assignments.contains(a)) {
							if (t.range(domain.types).contains(a))
								exist_quantifier(p, pos + 1, assignments)
							else
								RawFF()
						} else {

							for (value <- t.range(domain.types)){
								x_list = exist_quantifier(p,pos+1,assignments+(a->value)) :: x_list
							}
							combine_in_or(x_list.filter(_!=RawFF()))
						}
					case a: AtomTerm => exist_quantifier(p,pos+1,assignments)
					case a: NumeralTerm => exist_quantifier(p,pos+1,assignments)
					case a: StringTerm => exist_quantifier(p,pos+1,assignments)

					case _ => RawFF()
				}
			}
		}

		def foreach_quantifier(p : org.icar.fol.Predicate, pos : Int, assignments : Map[VariableTerm,ConstantTerm]) : RawPredicate = {
			if (pos == p.terms.size) {
				val pred = p.to_ground(assignments)
				RawVar(direct(pred))

			} else {
				var x_list : List[RawPredicate] = List.empty
				val arg = p.terms(pos)
				arg match {
					case a: VariableTerm =>
						val t : DomainPredArguments = domain.get_predicate_arg_type(p.functional,pos)
						if (assignments.contains(a)) {
							if (t.range(domain.types).contains(a))
								exist_quantifier(p, pos + 1, assignments)
							else
								RawTT()
						} else {
							for (value <- t.range(domain.types)) {
								x_list = foreach_quantifier(p, pos + 1, assignments + (a -> value)) :: x_list
							}
							combine_in_and(x_list.filter(_ != RawTT()))
						}
					case a: AtomTerm => foreach_quantifier(p,pos+1,assignments)
					case a: NumeralTerm => foreach_quantifier(p,pos+1,assignments)
					case a: StringTerm => foreach_quantifier(p,pos+1,assignments)

					case _ => RawFF()
				}
			}
		}

		def combine_in_or(predicate_formulas: List[RawPredicate]) : RawPredicate = {
			if (predicate_formulas.size==0)
				RawFF()
			else if (predicate_formulas.size==1)
				predicate_formulas.head
			else
				RawDisj(predicate_formulas.head,combine_in_or(predicate_formulas.tail))
		}
		def combine_in_and(predicate_formulas: List[RawPredicate]) : RawPredicate = {
			if (predicate_formulas.size==0)
				RawFF()
			else if (predicate_formulas.size==1)
				predicate_formulas.head
			else
				RawConj(predicate_formulas.head,combine_in_and(predicate_formulas.tail))
		}

		/* CONVERTING PREDICATE FORMULA */
		f match {
			case org.icar.fol.GroundLiteral(p) => RawVar(direct(p))
			case org.icar.fol.AlwaysTrue() => RawTT()
			case org.icar.fol.AlwaysFalse() => RawFF()
			case org.icar.fol.Negation(sf) => RawNeg[RawPredicate](predicate_formula(sf))
			case org.icar.fol.Conjunction(terms) =>
				if (terms.size==2)
					RawConj(predicate_formula(terms.head),predicate_formula(terms.tail.head))
				else if (terms.size==1)
					predicate_formula(terms.head)
				else
					RawConj(predicate_formula(terms.head), predicate_formula(org.icar.fol.Conjunction(terms.tail)))
			case org.icar.fol.Disjunction(terms) =>
				if (terms.size==2)
					RawDisj(predicate_formula(terms.head),predicate_formula(terms.tail.head))
				else if (terms.size==1)
					predicate_formula(terms.head)
				else
					RawDisj(predicate_formula(terms.head), predicate_formula(org.icar.fol.Disjunction(terms.tail)))
			case org.icar.fol.ExistQuantifier(p,vars) =>
				exist_quantifier(p,0,Map.empty)
			case org.icar.fol.UnivQuantifier(p,vars) =>
				foreach_quantifier(p,0,Map.empty)
			case _ => RawFF()
		}

	}


	def ltl_formula(f:HighLevel_LTLformula) : RawLTL = {
		f match {
			case org.icar.pmr_solver.Predicate(p) => RawVar(direct(p))
			case org.icar.pmr_solver.True() => RawTT()
			case org.icar.pmr_solver.Empty() => RawTT()         /* check redundancy */
			case org.icar.pmr_solver.False() => RawFF()
			case org.icar.pmr_solver.Implication(l,r) => RawImpl[RawLTL](ltl_formula(l),ltl_formula(r))
			case org.icar.pmr_solver.BiImplication(l,r) => RawIff[RawLTL](ltl_formula(l),ltl_formula(r))
			case org.icar.pmr_solver.Negation(op) => RawNeg[RawLTL](ltl_formula(op))
			case org.icar.pmr_solver.Conjunction(l,r) => RawConj[RawLTL](ltl_formula(l),ltl_formula(r))
			case org.icar.pmr_solver.Disjunction(l,r) => RawDisj[RawLTL](ltl_formula(l),ltl_formula(r))
			case org.icar.pmr_solver.Globally(op) => RawGlobally(ltl_formula(op))
			case org.icar.pmr_solver.Finally(op) => RawFinally(ltl_formula(op))
			case org.icar.pmr_solver.Next(op) => RawNext(ltl_formula(op))
			case org.icar.pmr_solver.Until(l,r) => RawUntil(ltl_formula(l),ltl_formula(r))
			case org.icar.pmr_solver.Release(l,r) => RawRelease(ltl_formula(l),ltl_formula(r))

			case _ => RawFF()
		}
	}


	def grounding_scenario(name : String, probability : Float, evo : Array[EvoOperator], assigned:Map[String,ConstantTerm]=Map.empty) : RawEvolution = {
		var raw_op_list : List[RawEvoOperator] = List.empty
		for (op <- evo)
			op match {
				case Deprec_AddEvoOperator(pred) => raw_op_list = RawAdd(RawVar(direct(pred))) :: raw_op_list
				case Deprec_RemoveEvoOperator(pred) => raw_op_list = RawRem(RawVar(direct(pred))) :: raw_op_list
				case Deprec_RemoveAllEvoOperator(pred_class) =>
					for (i <- 0 until inverse.size if inverse(i).functional==pred_class)
						raw_op_list = RawRem(RawVar(i)) :: raw_op_list
				case AddOperator(p) =>
					val p1 = HighLevel_PredicateFormula.substitution(p,assigned)
					val opt_p2 = p1.get_grounded
					if (opt_p2.isDefined) {
						val p2 = opt_p2.get
						raw_op_list = RawAdd(RawVar(direct(p2))) :: raw_op_list
					}

				case RmvOperator(p) =>
					val p1 = HighLevel_PredicateFormula.substitution(p,assigned)
					val opt_p2 = p1.get_grounded
					if (opt_p2.isDefined) {
						val p2 = opt_p2.get
						raw_op_list = RawRem(RawVar(direct(p2))) :: raw_op_list
					}

				case _ =>
			}
		RawEvolution(name,probability,raw_op_list.toArray)
	}

	def system_action(sys_action : SystemAction) : List[RawAction] = {

		def create_instances(to_assign:List[DomainPredArguments],assigned:Map[String,ConstantTerm]):List[RawAction] = {
			if (to_assign.isEmpty) {

				val real_pre = org.icar.fol.Conjunction(sys_action.pre,org.icar.fol.Negation(sys_action.post))
				val raw_precond = predicate_formula(HighLevel_PredicateFormula.substitution(real_pre,assigned))
				val raw_effects = for (e<-sys_action.effects) yield grounding_scenario(e.name,1,e.evo,assigned)

				List(RawAction(
					sys_action.id,
					raw_precond,
					raw_effects
				))

			} else {
				var to_instantiate : List[RawAction] = List.empty
				val arg : DomainPredArguments = to_assign.head
				if (arg.isInstanceOf[DomainVariable]) {
					for (value <- arg.range(domain.types)) {
						val variable = arg.asInstanceOf[DomainVariable]
						to_instantiate = create_instances(to_assign.tail, assigned + (variable.name -> value)) ::: to_instantiate
					}
				} else {
						to_instantiate = create_instances(to_assign.tail,assigned) ::: to_instantiate
				}

				to_instantiate
			}
		}

		create_instances(sys_action.params,Map.empty)
	}

	def environment_action(env_action : EnvironmentAction) : RawAction = {
		RawAction(
			env_action.id,
			predicate_formula(env_action.pre),
			for (e<-env_action.effects) yield grounding_scenario(e.name,e.probability,e.evo)
		)
	}

	//def axiom(ass:Assumption)
}
