package org.icar.pmr_solver

import net.sf.tweety.logics.fol.syntax.{FolFormula => TweetyF}
import org.icar.musa.context.EvoOperator



case class AvailableActions(sys_action : Array[SystemAction], env_action : Array[EnvironmentAction])




abstract class PlanningAction(pre:TweetyF)



case class SystemAction(
             id : String,
             cap : Capability,
             param_grounding : ParamsGrounding,
             pre : TweetyF,
             effects : Array[EvolutionGrounding]
           ) extends PlanningAction(pre)

case class EnvironmentAction(
                         id : String,
                         pert : Perturbation,
                         param_grounding : ParamsGrounding,
                         pre : TweetyF,
                         effects : Array[ProbabilisticEvolutionGrounding]
            ) extends PlanningAction(pre)


case class EvolutionGrounding(name : String, evo : Array[EvoOperator])
case class ProbabilisticEvolutionGrounding(name : String, probability : Float, evo : Array[EvoOperator])

case class ParamsGrounding(setting : Array[Grounding])

case class Grounding(variable : String, value : String)

