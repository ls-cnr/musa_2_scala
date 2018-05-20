package org.icar.musa.pmr

import org.icar.musa.context.StateOfWorld
import org.icar.musa.spec.AbstractCapability

import scala.collection.mutable.ArrayBuffer

class SolutionBuilder(start: WTSStateNode) {
  // to allow working with strings, the class need three maps:
  var state_map: Map[StateOfWorld, String] = Map() // state map
  var cap_map: Map[(String, String), AbstractCapability] = Map() // capability map
  var scenario_map: Map[String, Array[String]] = Map() // scenario map

  var partial: Set[Sequence] = Set()
  var complete: Set[Sequence] = Set()   //ONLY FOR TEST PURPOSE

  var solution_stack : Set[Solution] = Set()
  var complete_solution : Set[Solution] = Set()
  var new_solutions : List[Solution] = List()

  var state_counter: Int = 0
  var xor_counter: Int = 0

  init

  private def init(): Unit = {
    val name = add_state(start)
    val seq = ArrayBuffer[String](name)
    partial = partial + Sequence(seq, loop = false, exit = start.su.isAccepted)
  }

  private def add_state(state: WTSStateNode): String = {
    val opt = state_map.get(state.w)
    if (opt.isDefined)
      state_map(state.w)

    else {

      val name = "s" + state_counter
      state_counter += 1

      state_map = state_map + (state.w -> name)

      name
    }
  }

  def add_xor(scen: Array[String]): String = {
    var name = ""
    for (scen_name <- scenario_map.keys if scenario_map(scen_name).deep == scen.deep)
      name = scen_name

    if (name == "") {
      name = "X" + xor_counter
      scenario_map += (name -> scen)

      xor_counter += 1
    }

    name
  }

  def deal_with_expansion(exp: SimpleWTSExpansion): Unit = {
    val start_name = add_state(exp.start)
    val end_name = add_state(exp.end)
    println("from "+start_name+" to "+end_name+" with "+exp.cap.name)
    cap_map += ((start_name, end_name) -> exp.cap)

    update_seq_with(start_name, end_name, exp.end.su.isAccepted)
  }

  def deal_with_multi_expansion(exp: MultiWTSExpansion): Unit = {
    val start_name = add_state(exp.start)
    val scen = exp.evo.keys.toArray[String]

    val xor_name = add_xor(scen)
    //println("from "+start_name+" to "+xor_name+" with "+exp.cap.name)

    cap_map += ((start_name, xor_name) -> exp.cap)
    //update_seq_with(start_name, xor_name, exit = false)

    for (scen <- exp.evo) {
      val scen_name = "x." + scen._1
      //val scen_name = xor_name+"."+scen._1
      //partial = partial + Sequence(ArrayBuffer[String](scen_name),false,false)

      //update_seq_with(xor_name, scen_name, exit = false)

      val end_name = add_state(scen._2)
      println("from "+xor_name+" to "+end_name+" (via "+scen_name+")")
      cap_map += ((xor_name, end_name) -> exp.cap)
      //update_seq_with(scen_name, end_name, scen._2.su.isAccepted)

      update_seq_with_quadruple(start_name,xor_name,scen_name,end_name,scen._2.su.isAccepted)
    }
  }

  def update_seq_with(start_name: String, end_name: String, exit: Boolean = false): Unit = {
    var to_remove: List[Sequence] = List()
    var to_add: List[Sequence] = List()

    for (s <- partial) {
      val start_ind = s.seq.indexOf(start_name)
      val end_ind = s.seq.indexOf(end_name)


      val start_is_last = (s.seq.last == start_name) & (end_ind == -1)
      val start_is_contained = s.seq.contains(start_name) && (end_ind == -1) && !start_is_last
      val end_after_start = (start_ind != -1 & end_ind != -1) & end_ind > start_ind
      val end_before_start = (start_ind != -1 & end_ind != -1) & end_ind < start_ind

      if (start_is_last & !s.exit & !s.loop) {
        val (rmv, add) = concat_seq(s, start_name, end_name, exit)
        to_remove ++= rmv
        to_add ++= add
      } else if (start_is_contained) {
        val (rmv, add) = trunk_duplicate_seq(s, start_name, end_name, exit)
        to_remove ++= rmv
        to_add ++= add
      } else if (end_after_start) {
        val (rmv, add) = jump_duplicate_seq(s, start_name, end_name, exit)
        to_remove ++= rmv
        to_add ++= add
      } else if (end_before_start) {
        val (rmv, add) = loop_duplicate_seq(s, start_name, end_name, exit)
        to_remove ++= rmv
        to_add ++= add
      }

    }

    partial --= to_remove
    to_add = to_add.sortBy((x: Sequence) => x.seq.size).reverse

    for (s <- to_add)
      add_sequence(s)

  }

  private def add_sequence(sequence: Sequence): Unit = {
    var add: Boolean = true
    for (s <- partial)
      if (is_subset(sequence, s))
        add = false

    if (add) {
      partial += sequence
      if (sequence.exit || sequence.loop)
        add_complete(sequence)
    }
  }

  private def add_complete(sequence: Sequence): Unit = {
    complete += sequence      //ONLY FOR TEST PURPOSE

    if (cap_map.nonEmpty) {   //ONLY FOR TEST PURPOSE
      // search for new solutions
      val s = Solution.build_from_simple_sequence(sequence,cap_map)
      if (s.isDefined)
        add_solution_to_stack(s.get)
      else {
        val s2 = Solution.build_from_xor_sequence(sequence,cap_map,scenario_map)
        if (s2.isDefined)
          add_solution_to_stack(s2.get)
      }
    }
  }

  private def add_solution_to_stack(s : Solution) : Unit = {
    var to_add = List[Solution](s)

    // COMMENTATO PER SCOPI DI TEST
    for (sol <- solution_stack) {
      val merge = Solution.merge_xor_sequences(sol,s)
      if (merge.isDefined)
        to_add = merge.get :: to_add
    }

    for (sol <- to_add) {
      solution_stack += sol
      if (sol.check_completeness && s.check_soundness) {
        complete_solution += sol
        new_solutions = sol :: new_solutions
      }
    }
  }

  private def is_subset(s1: Sequence, s2: Sequence): Boolean = {
    var eq: Boolean = true
    if (s1.seq.size > s2.seq.size)
      eq = false
    else
      for (i <- s1.seq.indices)
        if (s1.seq(i) != s2.seq(i))
          eq = false

    eq
  }

  /*
  this rule applies when 'start' is the last element of the sequence
  Example: ABCS, S->E, ==> ABCSE
  */
  def concat_seq(s: Sequence, start_name: String, end_name: String, exit_flag: Boolean): (List[Sequence], List[Sequence]) = {
    var add_list = List[Sequence]()

    var newseq = s.seq.clone
    newseq += end_name

    if (end_name.startsWith("X") || end_name.startsWith("x")) {
      add_list = Sequence(newseq, loop = false, exit = exit_flag) :: add_list

    } else {
      // forward part
      val conclusions: List[Sequence] = search_conclusions(end_name)
      if (conclusions.isEmpty)
        add_list = Sequence(newseq, loop = false, exit = exit_flag) :: add_list
      else {
        for (c <- conclusions) {
          var sequence = newseq.clone()
          var loop = false
          for (i <- c.seq.indices if loop == false) {
            val ch = c.seq(i)
            sequence += ch
            if (newseq.contains(ch))
              loop = true
          }
          add_list = Sequence(sequence, loop, exit_flag) :: add_list
        }
      }
    }

    (List(s), add_list)
  }

  /*
  this rule applies when 'start' is conatined in the sequence but it is not the last element
  Example: ASCB, S->E, ==> ASCB + ASE
  */
  def trunk_duplicate_seq(s: Sequence, start_name: String, end_name: String, exit: Boolean): (List[Sequence], List[Sequence]) = {
    var newseq = trunk_after(s.seq, start_name)
    newseq += end_name

    (List(), List(Sequence(newseq, loop = false, exit = exit)))
  }

  /*
  this rule applies when 'end' is contained after 'start' in the sequence
  Example: ABSCED, S->E, ==> ABSCED + ABSED
  */
  def jump_duplicate_seq(s: Sequence, start_name: String, end_name: String, exit: Boolean): (List[Sequence], List[Sequence]) = {
    var newseq = trunk_after(s.seq, start_name)
    newseq ++= trunk_before(s.seq, end_name)

    (List(), List(Sequence(newseq, loop = false, exit = exit)))
  }

  /*
  this rule applies when 'end' is contained before 'start' in the sequence
  Example: AEBSC, S->E, ==> AEBSC + AEBSE(Loop)
  */
  def loop_duplicate_seq(s: Sequence, start_name: String, end_name: String, exit: Boolean): (List[Sequence], List[Sequence]) = {
    var rmv = List[Sequence]()
    var newseq = trunk_after(s.seq, start_name)
    if (newseq == s.seq)
      rmv = List(s)
    newseq += end_name

    //newseq += start_name

    (rmv, List(Sequence(newseq, loop = true, exit = false)))
  }

  /*
  when a new item is concatented to a sequence, it is necessaty to check for forward sequences
  example
  S0BC, S0A - A->B - S0BC, S0AB(C) the last C is added as forwarding the first sequence

  */
  private def search_conclusions(name: String): List[Sequence] = {
    var l = List[Sequence]()
    for (s <- partial) {
      val opt: Option[Sequence] = get_optional_forwarding(s, name)
      if (opt.isDefined)
        l = opt.get :: l
    }

    l
  }

  private def get_optional_forwarding(s: Sequence, name: String): Option[Sequence] = {
    val index = s.seq.indexOf(name)
    if (index != -1) {
      val a = s.seq.clone.drop(index + 1)
      if (a.nonEmpty)
        Some(s.copy(seq = a))
      else
        None
    } else
      None
  }


  private def trunk_after(original: ArrayBuffer[String], element: String): ArrayBuffer[String] = {
    var newseq = ArrayBuffer[String]()
    var i = 0
    var drop = false
    while (!drop) {
      val ch = original(i)
      newseq += ch
      if (ch == element)
        drop = true
      i += 1
    }
    newseq
  }

  private def trunk_before(original: ArrayBuffer[String], element: String): ArrayBuffer[String] = {
    var newseq = ArrayBuffer[String]()
    var drop = true
    for (i <- original.indices) {
      val ch = original(i)
      if (ch == element)
        drop = false
      if (!drop)
        newseq += ch
    }
    newseq
  }


  def log_state(): Unit = {
    if (partial.nonEmpty) {
      println("----- partial -----")
      partial.foreach(println(_))
    }
    if (complete.nonEmpty) {            //ONLY FOR TEST PURPOSE
      println("----- complete -----")
      complete.foreach(println(_))
    }

  }

  def log_mapping () : Unit = {
    println("where: ")
    for (s <- state_map.keys)
      println(state_map(s)+"="+s)
    for (c <- cap_map.keys)
      println(c+"<="+cap_map(c).name)


  }

}


