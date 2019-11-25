package org.icar.application.shipboard_power_system

import scala.io.Source
import scala.util.parsing.combinator.JavaTokenParsers

case class CircuitSpec(
	                      connections : List[(Int,Int)],
	                      loads : List[(String,Int,Int)],
	                      gens : List[(String,Int,Int)],
	                      switchers : List[(String,Int,Int)],
	                      mutex : List[(String,String)]
                      )



class SimulinkCircuitParser extends JavaTokenParsers {

  def circuit_spec : Parser[CircuitSpec] = node_spec~load_spec~gen_spec~switch_spec~mutex_spec ^^ {case node_spec~load_spec~gen_spec~switch_spec~mutex_spec => CircuitSpec(node_spec,load_spec,gen_spec,switch_spec,mutex_spec)}

  def node_spec : Parser[List[(Int,Int)]] = "[node-node]"~>rep(node_item)

  def node_item : Parser[(Int,Int)] = floatingPointNumber~","~floatingPointNumber ^^ { case id1~virg~id2 => (id1.toInt,id2.toInt)}

  def load_spec : Parser[List[(String,Int,Int)]] = "[load-node-pwr]"~>rep(item)

  def gen_spec : Parser[List[(String,Int,Int)]] = "[gen-node-pwr]"~>rep(item)

  def switch_spec : Parser[List[(String,Int,Int)]] = "[switch-node-node]"~>rep(item)

  def item : Parser[(String,Int,Int)] = ident~","~floatingPointNumber~","~floatingPointNumber ^^ { case id~virg1~num1~virg2~num2 => (id,num1.toInt,num2.toInt)}

  def mutex_spec : Parser[List[(String,String)]] = "[mutex]"~>rep( mutex_item )

  def mutex_item : Parser[(String,String)] = ident~","~ident ^^ { case id1~virg~id2 => (id1,id2)}
}



object SimulinkParser extends SimulinkCircuitParser {

	def load_circuit_spec(file:String) : CircuitSpec = {
		val s = Source.fromFile(file)
		val pp = parseAll(circuit_spec,s.mkString)
		val circ : CircuitSpec = pp.get
		circ
	}


	def main(args : Array[String]): Unit = {
	    val file = "./data/sps_data/circuit3.txt"
	    val circ : CircuitSpec = load_circuit_spec(file)
	    for (n <- circ.connections)
	      println("Connection from Node"+n._1+" to Node"+n._2)

	    for (n <- circ.loads)
	      println("Load: "+n._1+" attached to Node"+n._2+" with pow "+n._3)

	}
}

