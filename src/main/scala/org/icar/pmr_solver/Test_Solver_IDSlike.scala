package org.icar.pmr_solver

import org.icar.fol.{Assumption, AtomTerm, GroundPredicate, Literal, TweetyFormula}
import org.icar.musa.context.{AddEvoOperator, EvoOperator, RemoveEvoOperator, StateOfWorld}


// TO fix: state [registered(doc),rejected(doc),unavailable(doc),worked(doc)] is considered Exit?
// Explanation: An exit node is (for definition) a node where next()=Empty.
// So the behavior is actually correct (the Finally holds). All successors will be ok by definition.

// BUT Another problem: if I use a goal like Globally(...)... it will be never be Empty!!!
// it is necessary to change the definition of Exit goal.

object Test_Solver_IDSlike extends App {

	/* the domain */
	val axioms: Array[Assumption] =Array(Assumption("ready(X) :- available(X), registered(X)."))
	def qos(n:Node):Float=0
	val my_domain = Domain(Array.empty,Array.empty,axioms,qos)


	/* capability */
	val evo_register = Array(
		EvolutionGrounding("base",Array[EvoOperator](AddEvoOperator(GroundPredicate("registered", AtomTerm("doc")))
		)))
	val pre_register = TweetyFormula.fromFormula(
		org.icar.fol.Conjunction(
			Literal(org.icar.fol.Predicate("available", AtomTerm("doc"))),
			org.icar.fol.Negation(Literal(org.icar.fol.Predicate("registered", AtomTerm("doc"))))
		))
	val register = SystemAction("register", pre_register, evo_register)
	val manual_register = SystemAction("manual_register", pre_register, evo_register)

	val evo_work = Array(
		EvolutionGrounding("base",Array[EvoOperator](
			RemoveEvoOperator(GroundPredicate("to_modify", AtomTerm("doc"))),
			AddEvoOperator(GroundPredicate("worked", AtomTerm("doc"))),
			AddEvoOperator(GroundPredicate("to_be_revised", AtomTerm("doc")))
		)))
	val pre_work = TweetyFormula.fromFormula(
		org.icar.fol.Disjunction (
			org.icar.fol.Conjunction(
				Literal(org.icar.fol.Predicate("ready", AtomTerm("doc"))),
				Literal(org.icar.fol.Predicate("to_modify", AtomTerm("doc")))
			),
			org.icar.fol.Conjunction(
				Literal(org.icar.fol.Predicate("ready", AtomTerm("doc"))),
				org.icar.fol.Negation(Literal(org.icar.fol.Predicate("worked", AtomTerm("doc"))))
			)
		))
	val work = SystemAction("work", pre_work, evo_work)

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


	val sys_action = Array(register,work,request_again,supervise) //



	/* perturbations */
	val evo_lose = Array(
		ProbabilisticEvolutionGrounding("base",0.01f,Array[EvoOperator](RemoveEvoOperator(GroundPredicate("available", AtomTerm("doc"))), AddEvoOperator(GroundPredicate("unavailable", AtomTerm("doc")))))
	)
	val pre_lose = TweetyFormula.fromFormula(Literal(org.icar.fol.Predicate("accepted", AtomTerm("doc"))))
	val lose_doc = EnvironmentAction("lose",pre_lose,evo_lose)

	val env_action : Array[EnvironmentAction] = Array(lose_doc) //Array.empty


	/* the problem */
	val initial = StateOfWorld.create(GroundPredicate("available", AtomTerm("doc")))
	val accepted = GroundPredicate("accepted", AtomTerm("doc"))
	val rejected = GroundPredicate("rejected", AtomTerm("doc"))
	val available_doc = GroundPredicate("available", AtomTerm("doc"))
	val goalmodel = LTLGoalSet(Array(
		Finally(
			Conjunction(
				Predicate(available_doc),
				Disjunction(Predicate(accepted),Predicate(rejected))
			)
		)
	))
	val available = AvailableActions(sys_action,env_action)
	val my_problem = Problem(initial,goalmodel,available)


	/* the solver */
	val solver = new Solver(my_problem,my_domain)

	val its=solver.iterate_until_termination(IterationTermination(20))

	println("Number of iterations: "+its)
	println("Number of generated WTS: "+solver.solution_set.wts_list.size)
	println("Number of full WTS: "+solver.solution_set.full_wts.size)
	println("Number of partial WTS: "+solver.solution_set.partial_wts.size)

	println( solver.solution_set.all_solutions_to_graphviz(node => node.w.toString) )


}