package org.icar.application.shipboard_power_system

import org.icar.pmr_solver.high_level_specification.{AtomTerm, GroundPredicate, IntegerTerm}

abstract class ElectricNode {
	def up_condition : GroundPredicate
	def node_string : String
}
case class SimpleNode(id:Int) extends ElectricNode {
	override def up_condition: GroundPredicate = GroundPredicate("up_node",List(IntegerTerm(id)))
	override def node_string: String = s"N$id"
}
case class Load(id : Int,name:String,pow:Float) extends ElectricNode {
	override def up_condition: GroundPredicate = GroundPredicate("up_load",List(AtomTerm(name)))
	override def node_string: String = s"L$name"
}
case class Generator(id : Int,name:String,pow:Float) extends ElectricNode {
	override def up_condition: GroundPredicate = GroundPredicate("on_gen",List(AtomTerm(name)))
	override def node_string: String = s"G$name"
}

case class Switcher(id: Int, name:String, source : ElectricNode, dest : ElectricNode) {
	def closed_predicate : GroundPredicate = GroundPredicate("closed_sw",List(AtomTerm(name)))
}

case class TwoWaySelector(id: Int, name:String, pos1 : ElectricNode, middle : ElectricNode, pos2 : ElectricNode) {
	def pos1_predicate : GroundPredicate = GroundPredicate("pos1_sel",List(AtomTerm(name)))
	def pos2_predicate : GroundPredicate = GroundPredicate("pos2_sel",List(AtomTerm(name)))
}
case class ConnectionWithFailure(id:Int, source : ElectricNode, dest : ElectricNode) {
	def fired_predicate : GroundPredicate = GroundPredicate("failure",List(IntegerTerm(id)))
}