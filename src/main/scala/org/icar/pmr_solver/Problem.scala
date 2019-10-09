package org.icar.pmr_solver

import org.icar.musa.context.StateOfWorld

class Problem(val I : StateOfWorld, val goal_model : GoalModel, val cap_set : Array[Perturbation], val per_set: Array[Perturbation]) {

}
