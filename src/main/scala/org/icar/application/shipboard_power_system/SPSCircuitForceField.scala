package org.icar.application.shipboard_power_system

import org.icar.pmr_solver.symbolic_level.{HL2Raw_Map, RawState, RawVar}



class ForceField(circuit : SPSCircuit, mission : SPSMission, map:HL2Raw_Map) {

	var layers : List[ForceFieldLayer] = List.empty

	for (vital_id<-mission.vitals) {
		val vital = circuit.getLoadByName(vital_id)
		if (vital.isDefined){
			layers = ForceFieldLayer.build_by_load(circuit,vital.get,map) :: layers
		}
	}

	def qos(w:RawState ) : Float = {
		var sum : Float = 0

		for (layer <- layers)
			for (key <- layer.map.keys) {
				if (w.bit_descr(key.index))
					sum += layer.map(key)
			}


		sum
	}

}

case class ForceFieldLayer(map:Map[RawVar,Float])

object ForceFieldLayer {

	def build_by_load(circuit : SPSCircuit , focus : Load, map:HL2Raw_Map) : ForceFieldLayer = {
		var force_field : Map[RawVar,Float] = Map.empty

		explore(focus,1)

		def explore(node : ElectricNode, value : Float) : Unit = {
			val pred = node.up_condition
			val v = RawVar( map.direct(pred) )
			if (!force_field.contains(v) || force_field(v)<value) {

				force_field += (v -> value)

				val successors = get_successors(node)
				for (s <- successors)
					explore(s,value/2)

			}
		}


		def get_successors(node : ElectricNode) : List[ElectricNode] = {
			var next : List[ElectricNode] = List.empty

			for (s <- circuit.switchers)
				if (s.source==node)
					next = s.dest :: next
				else if (s.dest==node)
					next = s.source :: next

			for (c<-circuit.possible_failures)
				if (c.source==node)
					next = c.dest :: next
				else if (c.dest==node)
					next = c.source :: next

			next
		}

		ForceFieldLayer(force_field)
	}

	def merge_layers_by_sum(array:Array[ForceFieldLayer]) : ForceFieldLayer = {
		var merged_map:Map[RawVar,Float]=Map.empty

		if (array.length>0) {
			var nodes = array(1).map.keys

			for (n<-nodes) {
				var sum:Float=0
				for (f<-array)
					sum+=f.map(n)

				merged_map += (n->sum)
			}
		}
		ForceFieldLayer(merged_map)
	}

	def merge_layers_by_max(array:Array[ForceFieldLayer]) : ForceFieldLayer = {
		var merged_map:Map[RawVar,Float]=Map.empty

		if (array.length>0) {
			var nodes = array(1).map.keys

			for (n<-nodes) {
				var max:Float=0
				for (f<-array)
					if (f.map(n) > max)
						max = f.map(n)

				merged_map += (n->max)
			}
		}
		ForceFieldLayer(merged_map)
	}
}