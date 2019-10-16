package org.icar.pmr_solver

import org.icar.fol.{Assumption, AtomTerm, GroundPredicate, Literal, TweetyFormula}
import org.icar.musa.context.{AddEvoOperator, EvoOperator, RemoveEvoOperator, StateOfWorld}

object Test_Solver_OCCPlike extends App {
	val axioms: Array[Assumption] =Array(Assumption("ready(X) :- available(X), registered(X)."))
	def qos(n:Node):Float=0
	val my_domain = Domain(Array.empty,Array.empty,axioms,qos)
	val initial = StateOfWorld.create(GroundPredicate("available", AtomTerm("doc")))
	val accepted = GroundPredicate("accepted", AtomTerm("doc"))
	val rejected = GroundPredicate("rejected", AtomTerm("doc"))
	val goalmodel = LTLGoalSet(Array(Finally(Disjunction(Predicate(accepted),Predicate(rejected)))))

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
			Literal(org.icar.fol.Predicate("to_modify", AtomTerm("doc"))),
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
			Literal(org.icar.fol.Predicate("to_be_revised", AtomTerm("doc")))
		))
	val supervise = SystemAction("supervise", pre_supervise, evo_supervise)

	val sys_action = Array(register,manual_register,work,supervise)
	val available = AvailableActions(sys_action,Array.empty)
	val my_problem = Problem(initial,goalmodel,available)

	val solver = new Solver(my_problem,my_domain)

	val its=solver.iterate_until_termination(IterationTermination(5))

	println("Number of iterations: "+its)
	println("Number of generated WTS: "+solver.solution_set.wts_list.size)
	println("Number of full WTS: "+solver.solution_set.full_wts.size)
	println("Number of partial WTS: "+solver.solution_set.partial_wts.size)

	println( solver.solution_set.all_solutions_to_graphviz(node => node.w.toString) )


}
