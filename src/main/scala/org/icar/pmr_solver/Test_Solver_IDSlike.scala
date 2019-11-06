package org.icar.pmr_solver

import org.icar.fol.{Assumption, AtomTerm, GroundPredicate, Literal, StringTerm, TweetyFormula, VariableTerm}
import org.icar.musa.context.{AddOperator, Deprec_AddEvoOperator, Deprec_RemoveEvoOperator, EvoOperator, RmvOperator, StateOfWorld}
import org.icar.fol.{Predicate => folPre}
import org.icar.fol.{Negation => folNeg}
import org.icar.fol.{Conjunction => folConj}
import org.icar.fol.{Disjunction => folDisj}
import org.icar.fol.{ExistQuantifier => folExist}

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

	val dom_types : Array[DomainType] = Array(
		EnumerativeDomainType("doc_type",Array("issue_list","request","tech_rep")),
		EnumerativeDomainType("doc_state",Array("received","registered","worked","accepted","rejected","to_revise"))
	)

	val preds : Array[DomainPredicate] = Array(
		DomainPredicate("document",List(
			DomainVariable("TYPE","doc_type"),
			DomainVariable("STATE","doc_state")
		))
	)

	val my_domain = Domain(preds,dom_types,Array.empty,qos)


	/* capability */
	val register = SystemAction(
		id = "register",
		params = List(DomainVariable("TYPE","doc_type")),

		pre = folExist(
			folPre("document", VariableTerm("TYPE"), AtomTerm("received")),
			ArrayBuffer(VariableTerm("TYPE"))
		),

		post = folExist(
			folPre("document", VariableTerm("TYPE"), AtomTerm("registered")),
			ArrayBuffer(VariableTerm("TYPE"))
		),

		effects = Array(
			EvolutionGrounding("base",Array[EvoOperator](
				AddOperator(folPre("document", VariableTerm("TYPE"), AtomTerm("registered"))),
				RmvOperator(folPre("document", VariableTerm("TYPE"), AtomTerm("received")))
			)))
	)

	val work = SystemAction(
		id = "work",
		params = List(DomainVariable("TYPE","doc_type")),

		pre = folDisj (
			folExist(
				folPre("document", VariableTerm("TYPE"), AtomTerm("registered")),
				ArrayBuffer(VariableTerm("TYPE"))
			),
			folExist(
				folPre("document", VariableTerm("TYPE"), AtomTerm("to_revise")),
				ArrayBuffer(VariableTerm("TYPE"))
			)
		),

		post = folExist(
			folPre("document", VariableTerm("TYPE"), AtomTerm("worked")),
			ArrayBuffer(VariableTerm("TYPE"))
		),

		effects = Array(
			EvolutionGrounding("base",Array[EvoOperator](
				AddOperator(folPre("document", VariableTerm("TYPE"), AtomTerm("worked"))),
				RmvOperator(folPre("document", VariableTerm("TYPE"), AtomTerm("to_revise"))),
				RmvOperator(folPre("document", VariableTerm("TYPE"), AtomTerm("registered")))
			)))
	)

	val supervise = SystemAction(
		id = "supervise",
		params = List(DomainVariable("TYPE","doc_type")),

		pre = folExist(
			folPre("document", VariableTerm("TYPE"), AtomTerm("worked")),
			ArrayBuffer(VariableTerm("TYPE"))
		),

		post = folDisj (
			folExist(
				folPre("document", VariableTerm("TYPE"), AtomTerm("accepted")),
				ArrayBuffer(VariableTerm("TYPE"))
			),
			folExist(
				folPre("document", VariableTerm("TYPE"), AtomTerm("rejected")),
				ArrayBuffer(VariableTerm("TYPE"))
			),
			folExist(
				folPre("document", VariableTerm("TYPE"), AtomTerm("to_revise")),
				ArrayBuffer(VariableTerm("TYPE"))
			)
		),

		effects = Array(
			EvolutionGrounding("ok",Array[EvoOperator](
				AddOperator(folPre("document", VariableTerm("TYPE"), AtomTerm("accepted"))),
				RmvOperator(folPre("document", VariableTerm("TYPE"), AtomTerm("worked")))
			)),
			EvolutionGrounding("no",Array[EvoOperator](
				AddOperator(folPre("document", VariableTerm("TYPE"), AtomTerm("rejected"))),
				RmvOperator(folPre("document", VariableTerm("TYPE"), AtomTerm("worked")))
			)),
			EvolutionGrounding("change",Array[EvoOperator](
				AddOperator(folPre("document", VariableTerm("TYPE"), AtomTerm("to_revise"))),
				RmvOperator(folPre("document", VariableTerm("TYPE"), AtomTerm("worked")))
			))
		)
	)

	val sys_action = Array(register,work,supervise)//,work,request_again,supervise) //


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
