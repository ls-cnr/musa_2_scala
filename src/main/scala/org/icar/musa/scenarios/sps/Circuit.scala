package org.icar.musa.scenarios.sps

import org.icar.fol.{AtomTerm, GroundPredicate}
import org.icar.ltl.LogicAtom
import org.icar.musa.scenarios.sps.TestCircuitParser.{circuit_spec, parseAll}

import scala.collection.mutable.ArrayBuffer
import scala.io.Source



class Circuit {
  var nodes: Set[ElectricNode] = Set[ElectricNode]()
  var loads: ArrayBuffer[Load] = ArrayBuffer[Load]()
  var generators: ArrayBuffer[Generator] = ArrayBuffer[Generator]()
  var connections: ArrayBuffer[Connection] = ArrayBuffer[Connection]()
  var switcher: ArrayBuffer[Switcher] = ArrayBuffer[Switcher]()

  var sw_map: Map[String, String] = Map[String,String]()

  def getLoad(id:String):Option[Load] = {
    var ret : Option[Load] = None

    for (l<-loads if l.id==id)
      ret = Some(l)

    ret
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

  def node_map : Map[String, GroundPredicate] = {
    var map = Map[String, GroundPredicate]()
    for (n <- nodes)
      map += ("n"+n.id.toString -> n.up_cond)

    map
  }

  def print_for_graphviz_with_field(f:ForceFieldLayer) : Unit = {
    println("digraph Circuit {")

    for (n<-nodes) {
      println("N"+n.id+" [label=\"("+f.map(n)+")\"] ")
    }

    for (c <- connections) {
      println("N"+c.source.id + " -> N"+c.dest.id)
    }

    for (r <- switcher) {
      println("N"+r.source.id +" -> N"+r.dest.id+" [label=\""+r.id+"\"];")
    }

    for (l <- loads) {
      println(l.id+" [shape=invtriangle,color=black,label=\""+l.id+"\"];")
      println("N"+l.node.id +" -> "+l.id )
    }

    for (g <- generators) {
      println(g.id+" [shape=box,color=red];")
      println("N"+g.node.id +" -> "+g.id )
    }

    println("}")
  }

  def print_for_graphviz() : Unit = {
    println("digraph Circuit {")
    for (c <- connections) {
      println("N"+c.source.id + " -> N"+c.dest.id)
    }

    for (r <- switcher) {
      println("N"+r.source.id +" -> N"+r.dest.id+" [label=\""+r.id+"\"];")
    }

    for (l <- loads) {
      println(l.id+" [shape=invtriangle,color=black,label=\""+l.id+"\"];")
      println("N"+l.node.id +" -> "+l.id )
    }

    for (g <- generators) {
      println(g.id+" [shape=box,color=red];")
      println("N"+g.node.id +" -> "+g.id )
    }

    println("}")
  }
}




object Circuit {

  def load_from_file(file_name : String) : Circuit = {
    val circuit = new Circuit()
    var construction_map : Map[ElectricNode, ElectricNode] = Map.empty

    def safe_add_node(n:ElectricNode) : ElectricNode = {
      if (construction_map.contains(n))
        construction_map(n)
      else {
        circuit.nodes += n
        construction_map += (n -> n)
        n
      }
    }

    def safe_add_connection(src:ElectricNode, dst:ElectricNode) : Unit = {
      val src_is_new = !construction_map.contains(src)
      val dst_is_new = !construction_map.contains(dst)

      if (!src_is_new && !dst_is_new) {

        //println("adding connection "+src.id+" to "+dst.id)
        val src_map = construction_map(src)
        val dst_map = construction_map(dst)

        if (src_map != dst_map)
          circuit.connections += Connection(src,dst)

      } else if (!src_is_new && dst_is_new ) {

        val src_map = construction_map(src)
        //println("hiding node "+dst.id+" into "+src_map.id)
        construction_map += (dst -> src_map)

      } else if (src_is_new && !dst_is_new ) {

        val dst_map = construction_map(dst)
        //println("hiding node "+src.id+" into "+dst_map.id)
        construction_map += (src -> dst_map)

      } else {

        //println("adding node "+dst.id)
        //println("adding "+dst.id+" and hiding node "+src.id)

        circuit.nodes += dst
        construction_map += (dst -> dst)
        construction_map += (src -> dst)

      }

    }


    def add_connection(source : ElectricNode, dest : ElectricNode) : Unit = {
      safe_add_connection(source,dest)
    }

    def add_switcher(name : String, source : ElectricNode, dest : ElectricNode) : Switcher = {

      val src = safe_add_node(source)
      val dst = safe_add_node(dest)

      //println("connecting sw "+name+" between node "+src.id+" and "+dst.id)
      val s = Switcher( name,src,dst )
      circuit.switcher += s
      s
    }
    def add_load(id : String, node : ElectricNode) : Load = {

      val src = safe_add_node(node)

      //println("connecting load "+id+" to node "+src.id)
      val l = Load(id,src)
      circuit.loads += l
      l
    }

    def add_generator(id : String, node : ElectricNode) : Generator = {
      val src = safe_add_node(node)

      //println("connecting generator "+id+" to node "+src.id)
      val g = Generator(id,src)
      circuit.generators += g
      g
    }



    val s = Source.fromFile(file_name)
    val pp = parseAll(circuit_spec,s.mkString)

    val circ : CircuitSpec = pp.get
    for (n <- circ.nodes)
      add_connection(ElectricNode(n._1),ElectricNode(n._2))

    for (n <- circ.loads)
      add_load(n._1.toLowerCase,ElectricNode(n._2))

    for (s <- circ.switches)
      add_switcher(s._1.toLowerCase,ElectricNode(s._2),ElectricNode(s._3))

    for (g <- circ.gens)
      add_generator(g._1.toLowerCase,ElectricNode(g._2))

    for (m <- circ.mutex) {
      circuit.sw_map += (m._1.toLowerCase->m._2.toLowerCase)
      circuit.sw_map += (m._2.toLowerCase->m._1.toLowerCase)
    }

    circuit
  }


}




case class ElectricNode(id : Int) {
  def up : String = "up(n"+id+")"
  def down : String = "down(n"+id+")"

  def up_cond : GroundPredicate = GroundPredicate("up",AtomTerm("n"+id.toString))

}

case class Load(id : String, node : ElectricNode) {
  def up: String = "on("+id+")"
  def atom : LogicAtom = LogicAtom("on",AtomTerm(id))
  def up_cond : GroundPredicate = GroundPredicate("on",AtomTerm(id))
}

case class Generator(id : String, node : ElectricNode) {
  def up: String = "on("+id+")"
  def failure: String = "fail("+id+")"
  def up_cond : GroundPredicate = GroundPredicate("closed",AtomTerm("switchsw"+id))
}

case class Switcher(id: String, source : ElectricNode, dest : ElectricNode) {
  def closed: String = "closed("+id+")"
}

case class Connection(source : ElectricNode, dest : ElectricNode) {
  def failure: String = "f(c"+math.min(source.id,dest.id)+"_"+math.max(source.id,dest.id)+")"
  def failure_pred = GroundPredicate("f",AtomTerm("c"+math.min(source.id,dest.id)+"_"+math.max(source.id,dest.id)))
}
