package org.icar.pmr_solver

import org.icar.fol.{Assumption, AtomTerm, GroundPredicate, Literal, StringTerm, TweetyFormula, VariableTerm}
import org.icar.musa.context.{AddOperator, Deprec_AddEvoOperator, Deprec_RemoveEvoOperator, EvoOperator, RmvOperator, StateOfWorld}

import scala.collection.mutable.ArrayBuffer


// TO fix: state [registered(doc),rejected(doc),unavailable(doc),worked(doc)] is considered Exit?
// Explanation: An exit node is (for definition) a node where next()=Empty.
// So the behavior is actually correct (the Finally holds). All successors will be ok by definition.

// BUT Another problem: if I use a goal like Globally(...)... it will be never be Empty!!!
// it is necessary to change the definition of Exit goal.

object Test_Solver_IDSlike extends App {

	/* the domain */
	//val axioms: Array[Assumption] =Array(Assumption("ready(X) :- available(X), registered(X)."))
	def qos(n:RawState):Float=0

	val available_args : List[DomainPredArguments] = List(
		DomainVariable("TYPE",EnumerativeDomainType(Array("issue_list","request","tech_rep"))),
		DomainVariable("STATE",EnumerativeDomainType(Array("received","registered","worked","accepted","rejected","to_revise")))
	)

	val preds : Array[DomainPredicate] = Array(
		DomainPredicate("document",available_args)
	)

	val my_domain = Domain(preds,Array.empty,qos)


	/* capability */
	val evo_register = Array(
		EvolutionGrounding("base",Array[EvoOperator](
			AddOperator(org.icar.fol.Predicate("document", VariableTerm("TYPE"), AtomTerm("registered"))),
			RmvOperator(org.icar.fol.Predicate("document", VariableTerm("TYPE"), AtomTerm("received")))
		)))
	val pre_register = org.icar.fol.Conjunction(
			org.icar.fol.ExistQuantifier(
				org.icar.fol.Predicate("document", VariableTerm("TYPE"), AtomTerm("received")),
				ArrayBuffer(VariableTerm("TYPE"))
			),
			org.icar.fol.Negation(
				org.icar.fol.ExistQuantifier(
					org.icar.fol.Predicate("document", VariableTerm("TYPE"), AtomTerm("registered")),
					ArrayBuffer(VariableTerm("TYPE"))
				)
			)
		)
	val register = SystemAction("register", List(DomainVariable("TYPE",EnumerativeDomainType(Array("issue_list","request","tech_rep")))), pre_register, evo_register)


	val evo_work = Array(
		EvolutionGrounding("base",Array[EvoOperator](
			Deprec_AddEvoOperator(GroundPredicate("document", AtomTerm("tech_rep"), AtomTerm("worked"))),
			Deprec_RemoveEvoOperator(GroundPredicate("document", AtomTerm("tech_rep"), AtomTerm("to_revise")))
		)))
	val pre_pre_work = org.icar.fol.Disjunction (
			org.icar.fol.ExistQuantifier(
				org.icar.fol.Predicate("document", VariableTerm("TYPE"), AtomTerm("registered")),
				ArrayBuffer(VariableTerm("TYPE"))
			),
			org.icar.fol.ExistQuantifier(
				org.icar.fol.Predicate("document", VariableTerm("TYPE"), AtomTerm("to_revise")),
				ArrayBuffer(VariableTerm("TYPE"))
			)
		)
	val pre_work = org.icar.fol.Conjunction(
		pre_pre_work,
		org.icar.fol.Negation(
			org.icar.fol.ExistQuantifier(
				org.icar.fol.Predicate("document", VariableTerm("TYPE"), AtomTerm("worked")),
				ArrayBuffer(VariableTerm("TYPE"))
			)
		)
	)
	val work = SystemAction("work", List(DomainVariable("TYPE",EnumerativeDomainType(Array("issue_list","request","tech_rep")))), pre_work, evo_work)

	/*
	val evo_supervise = Array(
		EvolutionGrounding("accept",Array[EvoOperator](RemoveEvoOperator(GroundPredicate("to_be_revised", AtomTerm("doc"))), AddEvoOperator(GroundPredicate("accepted", AtomTerm("doc"))))),
		EvolutionGrounding("annotate",Array[EvoOperator](RemoveEvoOperator(GroundPredicate("to_be_revised", AtomTerm("doc"))), AddEvoOperator(GroundPredicate("to_modify", AtomTerm("doc"))))),
		EvolutionGrounding("reject",Array[EvoOperator](RemoveEvoOperator(GroundPredicate("to_be_revised", AtomTerm("doc"))), AddEvoOperator(GroundPredicate("rejected", AtomTerm("doc")))))
	)
	val pre_supervise = TweetyFormula.fromFormula(
		org.icar.fol.Conjunction(
			Literal(org.icar.fol.Predicate("ready", AtomTerm("doc"))),
			Literal(org.icar.fol.Predicate("to_be_revised", AtomTerm("doc")))
		))
	val supervise = SystemAction("supervise", pre_supervise, evo_supervise)


	val evo_request = Array(
		EvolutionGrounding("base",Array[EvoOperator](
			RemoveEvoOperator(GroundPredicate("registered", AtomTerm("doc"))),
			RemoveEvoOperator(GroundPredicate("unavailable", AtomTerm("doc"))),
			AddEvoOperator(GroundPredicate("available", AtomTerm("doc")))
		)))
	val pre_request = TweetyFormula.fromFormula(
			Literal(org.icar.fol.Predicate("unavailable", AtomTerm("doc")))
		)
	val request_again = SystemAction("request", pre_request, evo_request)
*/


	val sys_action = Array(register,work)//,work,request_again,supervise) //


//
//	/* perturbations */
//	val evo_lose = Array(
//		ProbabilisticEvolutionGrounding("base",0.01f,Array[EvoOperator](RemoveEvoOperator(GroundPredicate("available", AtomTerm("doc"))), AddEvoOperator(GroundPredicate("unavailable", AtomTerm("doc")))))
//	)
//	val pre_lose = TweetyFormula.fromFormula(Literal(org.icar.fol.Predicate("available", AtomTerm("doc"))))
//	val lose_doc = EnvironmentAction("lose",pre_lose,evo_lose)
//
	val env_action : Array[EnvironmentAction] = Array.empty// Array(lose_doc) //


	/* the problem */
	val initial = StateOfWorld.create(GroundPredicate("document", AtomTerm("tech_rep"),AtomTerm("received")))
	val accepted = GroundPredicate("document", AtomTerm("tech_rep"),AtomTerm("accepted"))
	val rejected = GroundPredicate("document", AtomTerm("tech_rep"),AtomTerm("rejected"))

	val goalmodel = LTLGoalSet(Array(

		Finally(
			Disjunction(Predicate(accepted),Predicate(rejected)
			)
		)

	))
	val available = AvailableActions(sys_action,env_action)
	val my_problem = Problem(initial,goalmodel,available)


	/* the solver */
	val solver = new Solver(my_problem,my_domain)

	val its=solver.iterate_until_termination(SolverConfiguration(IterationTermination(20),SolutionConfiguration(allow_self_loop = false, allow_cap_multiple_instance = true, allow_loop = true, allow_parallel_action = true)))

	if (solver.opt_solution_set.isDefined) {
		println("Number of iterations: "+its)
		println("Number of generated WTS: "+solver.opt_solution_set.get.wts_list.size)
		println("Number of full WTS: "+solver.opt_solution_set.get.full_wts.size)
		println("Number of partial WTS: "+solver.opt_solution_set.get.partial_wts.size)

		println( solver.opt_solution_set.get.all_solutions_to_graphviz(node => node.toString) )
	}

}
