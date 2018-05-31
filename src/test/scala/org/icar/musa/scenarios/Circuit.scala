package org.icar.musa.scenarios

import org.icar.fol.{AtomTerm, FOLCondition, GroundLiteral, GroundPredicate}
import org.icar.ltl.LogicAtom

import scala.collection.mutable.ArrayBuffer

case class Node(id : Int) {
  def up : String = "up(n"+id+")"
  def down : String = "down(n"+id+")"
}

case class Load(id : String, node : Node) {
  def up: String = "on("+id+")"
  def atom : LogicAtom = LogicAtom("on",AtomTerm(id))
  def up_cond : GroundPredicate = GroundPredicate("on",AtomTerm(id))
}

case class Generator(id : String, node : Node) {
  def up: String = "on("+id+")"
  def failure: String = "fail("+id+")"
  def up_cond : GroundPredicate = GroundPredicate("on",AtomTerm(id))
}

case class Switcher(id: String, source : Node, dest : Node) {
  def closed: String = "closed("+id+")"
}

case class Connection(source : Node, dest : Node) {
  def failure: String = "f(c"+math.min(source.id,dest.id)+"_"+math.max(source.id,dest.id)+")"
  def failure_pred = GroundPredicate("f",AtomTerm("c"+math.min(source.id,dest.id)+"_"+math.max(source.id,dest.id)))
}


class Circuit {
  var free_node_counter=10000

  var nodes = Set[Node]()
  var loads = ArrayBuffer[Load]()
  var generators = ArrayBuffer[Generator]()
  var connections = ArrayBuffer[Connection]()
  var switcher = ArrayBuffer[Switcher]()

  var sw_map = Map[String,String]()

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

  def load_conditions : Map[String, GroundPredicate] = {
    var map = Map[String, GroundPredicate]()
    for (l <- loads)
      map += (l.id -> l.up_cond)
    map
  }

  def gen_conditions : Map[String, GroundPredicate] = {
    var map = Map[String, GroundPredicate]()
    for (g <- generators)
      map += (g.id -> g.up_cond)
    map
  }

  def cond_map : Map[String, GroundPredicate] = {
    var map = Map[String, GroundPredicate]()
    map ++= load_conditions
    map ++= gen_conditions

    map
  }
}

object Circuit {
  def circuit3 : Circuit = {
    val c = new Circuit()

    c.add_connection(Node(1),Node(2))
    c.add_connection(Node(2),Node(3))
    c.add_connection(Node(3),Node(4))
    c.add_connection(Node(4),Node(5))
    c.add_connection(Node(5),Node(6))
    c.add_connection(Node(6),Node(7))
    c.add_connection(Node(7),Node(8))
    c.add_connection(Node(8),Node(9))

    c.add_connection(Node(1),Node(10))
    c.add_switcher("swp1",Node(10),Node(11))
    c.add_connection(Node(11),Node(12))
    c.add_switcher("sws1",Node(12),Node(13))
    c.add_connection(Node(13),Node(14))

    c.sw_map += ("swp1"->"sws1")
    c.sw_map += ("sws1"->"swp1")

    c.add_switcher("swaux1p",Node(2),Node(15))
    c.add_switcher("swaux1s",Node(15),Node(16))

    c.sw_map += ("swaux1p"->"swaux1s")
    c.sw_map += ("swaux1s"->"swaux1p")

    c.add_connection(Node(3),Node(17))
    c.add_switcher("swp2",Node(17),Node(18))
    c.add_connection(Node(18),Node(19))
    c.add_switcher("sws2",Node(19),Node(20))
    c.add_connection(Node(20),Node(21))

    c.sw_map += ("swp2"->"sws2")
    c.sw_map += ("sws2"->"swp2")

    c.add_switcher("swp3",Node(4),Node(22))
    c.add_switcher("sws3",Node(22),Node(24))
    c.add_connection(Node(24),Node(25))

    c.sw_map += ("swp3"->"sws3")
    c.sw_map += ("sws3"->"swp3")

    c.add_connection(Node(5),Node(26))
    c.add_switcher("swp4",Node(26),Node(27))
    c.add_connection(Node(27),Node(28))
    c.add_switcher("sws4",Node(28),Node(29))
    c.add_connection(Node(29),Node(30))

    c.sw_map += ("swp4"->"sws4")
    c.sw_map += ("sws4"->"swp4")

    c.add_switcher("swaux2p",Node(6),Node(31))
    c.add_switcher("swaux2s",Node(31),Node(32))

    c.sw_map += ("swaux2p"->"swaux2s")
    c.sw_map += ("swaux2s"->"swaux2p")

    c.add_connection(Node(7),Node(33))
    c.add_switcher("swp5",Node(33),Node(34))
    c.add_connection(Node(34),Node(35))
    c.add_switcher("sws5",Node(35),Node(36))
    c.add_connection(Node(36),Node(37))

    c.sw_map += ("swp5"->"sws5")
    c.sw_map += ("sws5"->"swp5")

    c.add_connection(Node(8),Node(38))
    c.add_switcher("swp6",Node(38),Node(39))
    c.add_switcher("sws6",Node(39),Node(41))

    c.sw_map += ("swp6"->"sws6")
    c.sw_map += ("sws6"->"swp6")

    c.add_connection(Node(9),Node(42))
    c.add_switcher("swp7",Node(42),Node(43))
    c.add_connection(Node(43),Node(44))
    c.add_switcher("sws7",Node(44),Node(45))
    c.add_connection(Node(45),Node(46))

    c.sw_map += ("swp7"->"sws7")
    c.sw_map += ("sws7"->"swp7")

    c.add_connection(Node(14),Node(16))
    c.add_connection(Node(16),Node(21))
    c.add_connection(Node(21),Node(25))
    c.add_connection(Node(25),Node(30))
    c.add_connection(Node(30),Node(32))
    c.add_connection(Node(32),Node(37))
    c.add_connection(Node(37),Node(41))
    c.add_connection(Node(41),Node(46))

    c.add_load("l2",Node(11))
    c.add_load("l6",Node(18))
    c.add_load("l9",Node(22))
    c.add_load("l12",Node(27))
    c.add_load("l16",Node(34))
    c.add_load("l19",Node(39))
    c.add_load("l22",Node(43))

    c.add_load("l3",Node(12))
    c.add_load("l7",Node(19))
    c.add_load("l13",Node(28))
    c.add_load("l17",Node(35))
    c.add_load("l23",Node(44))

    c.add_load("l1",Node(10))
    c.add_load("l4",Node(13))
    c.add_load("l5",Node(17))
    c.add_load("l8",Node(20))
    c.add_load("l11",Node(26))
    c.add_load("l14",Node(29))
    c.add_load("l15",Node(33))
    c.add_load("l18",Node(36))
    c.add_load("l21",Node(42))
    c.add_load("l24",Node(45))

    c.add_generator("mg1",Node(24))
    c.add_generator("mg2",Node(38))
    c.add_generator("aux1",Node(31))
    c.add_generator("aux2",Node(15))



    c
  }
}
