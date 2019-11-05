package org.icar.musa.scenarios

import org.icar.fol._
import org.icar.ltl.{Finally, Globally, LogicAtom, LogicDisjunction}
import org.icar.musa.context.{Deprec_AddEvoOperator, EvoOperator, StateOfWorld}
import org.icar.musa.pmr._
import org.icar.musa.main_entity.{AbstractCapability, EvolutionScenario, GroundedAbstractCapability, LTLGoal}

class OCCPScenario extends Scenario {

  override def initial_state: StateOfWorld = StateOfWorld.create(GroundPredicate("available", AtomTerm("an_order")))

  override def capabilities: Array[AbstractCapability] = {
    val c1 = check_user_capability
    val c2 = send_form_capability
    val c3 = wait_filled_form_capability
    val c4 = check_storehouse_capability
    val c5 = upload_on_user_cloud_capability
    val c6 = upload_on_company_cloud_capability
    val c7 = send_cloud_ref_capability
    val c8 = notify_storehouse_capability

    Array[AbstractCapability](c1,c2,c3,c4,c5,c6,c7,c8)
  }

  private def check_user_capability : GroundedAbstractCapability = {
    val pre = FOLCondition(Literal(Predicate("available", AtomTerm("an_order"))))
    val post = FOLCondition(Disjunction(Literal(Predicate("registered", AtomTerm("user"))),Literal(Predicate("unregistered", AtomTerm("user")))))
    val evo_1 = EvolutionScenario(Array[EvoOperator](Deprec_AddEvoOperator(GroundPredicate("registered", AtomTerm("user"))),Deprec_AddEvoOperator(GroundPredicate("has_cloud_space", AtomTerm("user")))))
    val evo_2 = EvolutionScenario(Array[EvoOperator](Deprec_AddEvoOperator(GroundPredicate("registered", AtomTerm("user")))))
    val evo_3 = EvolutionScenario(Array[EvoOperator](Deprec_AddEvoOperator(GroundPredicate("unregistered", AtomTerm("user")))))
    GroundedAbstractCapability("check_user",pre,post,Map("registered_with_cloud"-> evo_1, "registered_no_cloud" -> evo_2, "unregistered" -> evo_3))
  }

  private def send_form_capability : GroundedAbstractCapability = {
    val pre = FOLCondition(Literal(Predicate("unregistered", AtomTerm("user"))))
    val post = FOLCondition(Literal(Predicate("sent", AtomTerm("registration_form"),AtomTerm("user"))))
    val evo = EvolutionScenario(Array[EvoOperator](Deprec_AddEvoOperator(GroundPredicate("sent", AtomTerm("registration_form"),AtomTerm("user") ))))
    GroundedAbstractCapability("send_form",pre,post,Map("1"-> evo))
  }

  private def wait_filled_form_capability : GroundedAbstractCapability = {
    val pre = FOLCondition(Literal(Predicate("sent", AtomTerm("registration_form"),AtomTerm("user"))))
    val post = FOLCondition(Conjunction(Literal(Predicate("received", AtomTerm("filled_form"),AtomTerm("user"))), Literal(Predicate("registered", AtomTerm("user"))) ))
    val evo = EvolutionScenario(Array[EvoOperator](Deprec_AddEvoOperator(GroundPredicate("received", AtomTerm("filled_form"),AtomTerm("user") )), Deprec_AddEvoOperator(GroundPredicate("registered", AtomTerm("user") ))) )
    GroundedAbstractCapability("wait_filled_form",pre,post,Map("1"-> evo))
  }

  private def check_storehouse_capability : GroundedAbstractCapability = {
    val pre = FOLCondition(Conjunction(Literal(Predicate("available", AtomTerm("an_order"))), Literal(Predicate("registered", AtomTerm("user"))) ))
    val post = FOLCondition(Disjunction(Literal(Predicate("accepted", AtomTerm("an_order"))),Literal(Predicate("refused", AtomTerm("an_order")))))
    val evo_1 = EvolutionScenario(Array[EvoOperator](Deprec_AddEvoOperator(GroundPredicate("accepted", AtomTerm("an_order"))),Deprec_AddEvoOperator(GroundPredicate("invoice", AtomTerm("user"),AtomTerm("an_order")))))
    val evo_2 = EvolutionScenario(Array[EvoOperator](Deprec_AddEvoOperator(GroundPredicate("refused", AtomTerm("an_order")))))
    GroundedAbstractCapability("check_storehouse",pre,post,Map("accepted"-> evo_1, "refused" -> evo_2))
  }

  private def upload_on_user_cloud_capability : GroundedAbstractCapability = {
    val pre = FOLCondition(Conjunction(Literal(Predicate("invoice", AtomTerm("user"),AtomTerm("an_order"))), Literal(Predicate("has_cloud_space", AtomTerm("user"))) ))
    val post = FOLCondition(Literal(Predicate("notified", AtomTerm("invoice"),AtomTerm("user"))))
    val evo = EvolutionScenario(Array[EvoOperator](Deprec_AddEvoOperator(GroundPredicate("upload_on_cloud", AtomTerm("invoice"),AtomTerm("user"))), Deprec_AddEvoOperator(GroundPredicate("notified", AtomTerm("invoice"),AtomTerm("user"))) ))
    GroundedAbstractCapability("upload_on_user_cloud",pre,post,Map("1"-> evo))
  }

  private def upload_on_company_cloud_capability : GroundedAbstractCapability = {
    val pre = FOLCondition(Conjunction(Literal(Predicate("invoice", AtomTerm("user"),AtomTerm("an_order"))), Negation(Literal(Predicate("has_cloud_space", AtomTerm("user")))) ))
    val post = FOLCondition(Literal(Predicate("upload_on_cloud", AtomTerm("invoice"),AtomTerm("company"))))
    val evo = EvolutionScenario(Array[EvoOperator](Deprec_AddEvoOperator(GroundPredicate("upload_on_cloud", AtomTerm("invoice"),AtomTerm("company")))))
    GroundedAbstractCapability("upload_on_company_cloud",pre,post,Map("1"-> evo))
  }

  private def send_cloud_ref_capability : GroundedAbstractCapability = {
    val pre = FOLCondition(Conjunction(Literal(Predicate("invoice", AtomTerm("user"),AtomTerm("an_order"))), Literal(Predicate("upload_on_cloud", AtomTerm("invoice"),AtomTerm("company"))) ))
    val post = FOLCondition(Literal(Predicate("notified", AtomTerm("invoice"),AtomTerm("user"))))
    val evo = EvolutionScenario(Array[EvoOperator](Deprec_AddEvoOperator(GroundPredicate("sent", AtomTerm("invoice_ref"),AtomTerm("user"))), Deprec_AddEvoOperator(GroundPredicate("notified", AtomTerm("invoice"),AtomTerm("user"))) ))
    GroundedAbstractCapability("send_cloud_ref",pre,post,Map("1"-> evo))
  }

  private def notify_storehouse_capability : GroundedAbstractCapability = {
    val pre = FOLCondition(Literal(Predicate("notified", AtomTerm("invoice"),AtomTerm("user"))))
    val post = FOLCondition(Literal(Predicate("processed", AtomTerm("an_order"))))
    val evo = EvolutionScenario(Array[EvoOperator](Deprec_AddEvoOperator(GroundPredicate("processed", AtomTerm("an_order"))) ))
    GroundedAbstractCapability("notify_storehouse",pre,post,Map("1"-> evo))
  }

  override def termination: TerminationDescription = MaxEmptyIterationTermination(10)

  override def assumption_set: AssumptionSet = AssumptionSet()

  override def goal_specification: LTLGoal = LTLGoal(Finally(LogicDisjunction(LogicAtom("processed", AtomTerm("an_order")),LogicAtom("refused", AtomTerm("an_order")))))

  override def quality_asset: QualityAsset = new EmptyQualityAsset(AssumptionSet())
}
