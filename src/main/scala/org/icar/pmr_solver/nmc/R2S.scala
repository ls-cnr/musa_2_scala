package org.icar.pmr_solver.nmc

import org.icar.pmr_solver.symbolic_level.{RawConj, RawDisj, RawFF, RawFinally, RawGlobally, RawIff, RawImpl, RawLTL, RawNeg, RawNext, RawRelease, RawState, RawTT, RawUntil, RawVar}

object R2S {
	val Rinf : Float = 100000
	val Rmax : Float = 100
	val Rmin : Float = 1/100

	def metric(goal: RawLTL)(current: RawState) = calculate_resistance(current,goal)

	def calculate_resistance(current: RawState, goal: RawLTL): Float = {
		goal match {
			case v: RawVar => if (current.satisfies(v)) Rmin else Rmax
			case RawTT() => Rmin
			case RawFF() => Rinf

			case x:RawNeg[RawLTL] => 1/calculate_resistance(current,x.op)
			case x:RawConj[RawLTL] => calculate_resistance(current,x.left)+calculate_resistance(current,x.right)
			case x:RawDisj[RawLTL] => parallel(calculate_resistance(current,x.left),calculate_resistance(current,x.right))
			case x:RawImpl[RawLTL] => calculate_resistance(current,RawDisj(x.left,RawNeg(x.right)))
			case x:RawIff[RawLTL] => calculate_resistance(current,RawConj(RawDisj(x.left,RawNeg(x.right)),RawDisj(x.right,RawNeg(x.left))))

			case RawNext(op) => calculate_resistance(current,op)
			case RawFinally(op) => calculate_resistance(current,op)
			case RawGlobally(op) => calculate_resistance(current,op)
			case RawUntil(left,right) => calculate_resistance(current,left)+calculate_resistance(current,right)
			case RawRelease(left,right) => calculate_resistance(current,left)+calculate_resistance(current,right)

			case _ => Rinf
		}
	}

	def parallel(left: Float, right: Float): Float = {
		(left*right)/(left+right)
	}


}
