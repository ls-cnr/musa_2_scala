package scala.org.icar.symbolic_level

import org.icar.pmr_solver.symbolic_level._

object R2S {
	val Rinf : Float = 100000
	val Rmax : Float = 100
	val Rmin : Float = 1/100

	def metric(goal: RawLTL)(current: RawState) = calculate_resistance(current,goal)

	def calculate_goals_resistance(current: RawState, goals: Array[RawLTL]): Float = {
		var sum : Float = 0
		for (g<-goals) sum += calculate_resistance(current,g)
		sum
	}

	def calculate_resistance(current: RawState, goal: RawLTL): Float = {
		goal match {
			case v: RawVar => if (current.satisfies(v)) Rmin else Rmax
			case RawTT() => Rmin
			case RawFF() => Rinf

			case RawNeg(op) =>; 1/calculate_resistance(current,op.asInstanceOf)
			case RawConj(left, right) => calculate_resistance(current,left.asInstanceOf)+calculate_resistance(current,right.asInstanceOf)
			case RawDisj(left, right) => parallel(calculate_resistance(current,left.asInstanceOf),calculate_resistance(current,right.asInstanceOf))
			case RawImpl(left, right) => calculate_resistance(current,RawDisj(left,RawNeg(right)))
			case RawIff(left, right) => calculate_resistance(current,RawConj(RawDisj(left,RawNeg(right)),RawDisj(right,RawNeg(left))))

			case RawNext(op) => calculate_resistance(current,op)
			case RawFinally(op) => calculate_resistance(current,op)
			case RawGlobally(op) => calculate_resistance(current,op)
			case RawUntil(left, right) => calculate_resistance(current,left)+calculate_resistance(current,right)
			case RawRelease(left, right) => calculate_resistance(current,left)+calculate_resistance(current,right)

			case _ => Rinf
		}
	}

	private def parallel(left: Float, right: Float): Float = {
		(left*right)/(left+right)
	}


}
