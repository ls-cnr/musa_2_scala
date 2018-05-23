package org.icar.musa.scenarios

import scala.collection.mutable.ArrayBuffer

case class Node(id : Int) {
  def up : String = "up(n"+id+")"
  def down : String = "down(n"+id+")"
}

case class Load(id : String, node : Node) {
  def up: String = "up("+id+")"

}

case class Generator(id : String, node : Node) {
  def up: String = "up("+id+")"
  def failure: String = "fail("+id+")"
}

case class Switcher(id: String, source : Node, dest : Node) {
  def closed: String = "closed("+id+")"

}

case class Connection(source : Node, dest : Node) {
  def failure: String = "f(c"+math.min(source.id,dest.id)+"_"+math.max(source.id,dest.id)+")"

}


class Circuit {
  var free_node_counter=10000

  var nodes = Set[Node]()
  var loads = ArrayBuffer[Load]()
  var generators = ArrayBuffer[Generator]()
  var connections = ArrayBuffer[Connection]()
  var switcher = ArrayBuffer[Switcher]()

  def add_free_node : Node = {
    val n = Node(free_node_counter)
    free_node_counter += 1
    nodes += n
    n
  }

  def add_connection(source : Node, dest : Node) : Connection = {
    val c = Connection(source,dest)
    nodes += source
    nodes += dest
    connections += c
    c
  }
  def add_switcher(name : String, source : Node, dest : Node) : Switcher = {
    val s = Switcher(name,source,dest)
    nodes += source
    nodes += dest
    switcher += s
    s
  }
  def add_load(id : String, node : Node) : Load = {
    nodes += node
    val free_node = add_free_node
    val s = add_switcher("sw"+id,node,free_node)
    val l = Load(id,free_node)
    loads += l
    l
  }

  def add_generator(id : String, node : Node) : Generator = {
    nodes += node
    val g = Generator(id,node)
    generators += g
    g
  }

}
