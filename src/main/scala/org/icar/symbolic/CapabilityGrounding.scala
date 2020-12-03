package org.icar.symbolic

import org.icar.pmr_solver.high_level_specification.ConstantTerm

case class CapabilityGrounding(capability: AbstractCapability, grounding: Map[String, ConstantTerm]) {
	def unique_id: String = {
		var unique_id: String = capability.id

		if (grounding.nonEmpty) {
			unique_id += "("
			var first = true
			for (v <- grounding.keys)
				if (first) {
					unique_id += v + "=" + grounding(v)
					first = false
				} else {
					unique_id += "," + v + "=" + grounding(v)
				}

			unique_id += ")"
		}

		unique_id
	}
}
