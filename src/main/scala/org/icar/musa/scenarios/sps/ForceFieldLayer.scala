package org.icar.musa.scenarios.sps


class ForceField(circuit : Circuit, mission : Mission) {

	var layers : Map[Load,ForceFieldLayer] = Map.empty

	for (vital_label<-mission.vitals) {
		val vital = circuit.getLoad(vital_label)
		if (vital.isDefined)
			layers += (vital.get -> ForceFieldLayer.build_by_load(circuit,vital.get))
	}

	def getForceField(vital_id : String) : Option[ForceFieldLayer] = {
		val load = circuit.getLoad(vital_id)
		if (load.isDefined && layers.contains(load.get))
			Some(layers(load.get))
		else
			None
	}

}


case class ForceFieldLayer(map:Map[Node,Float])


object ForceFieldLayer {

	def build_by_load(circuit : Circuit , focus : Load) : ForceFieldLayer = {
		var force_field : Map[Node,Float] = Map.empty

		explore(focus.node,1)

		def explore(node : Node,value : Float) : Unit = {
			if (!force_field.contains(node) || force_field(node)<value) {

				force_field += (node -> value)

				val successors = get_successors(node)
				for (s <- successors)
					explore(s,value/2)

			}
		}


		def get_successors(node : Node) : List[Node] = {
			var next : List[Node] = List.empty

			for (c<-circuit.connections)
				if (c.source==node)
					next = c.dest :: next
				else if (c.dest==node)
					next = c.source :: next

			for (s <- circuit.switcher)
				if (s.source==node)
					next = s.dest :: next
				else if (s.dest==node)
					next = s.source :: next

			next
		}

		ForceFieldLayer(force_field)
	}

	def merge_layers_by_sum(array:Array[ForceFieldLayer]) : ForceFieldLayer = {
		var merged_map:Map[Node,Float]=Map.empty

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
		var merged_map:Map[Node,Float]=Map.empty

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

