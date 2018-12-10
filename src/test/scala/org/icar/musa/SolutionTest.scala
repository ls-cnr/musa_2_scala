package org.icar.musa

import junit.framework.TestCase
import org.icar.musa.pmr._
import org.icar.musa.main_entity.GroundedAbstractCapability

class SolutionTest extends TestCase {

  def testSimpleSolution (): Unit = {
    var s = new Solution()
    val t1 = WfTask( GroundedAbstractCapability("capAB",null,null,null,null) )
    val t2 = WfTask( GroundedAbstractCapability("capBxCD",null,null,null,null) )
    val t3 = WfTask( GroundedAbstractCapability("capDF",null,null,null,null) )
    val t4 = WfTask( GroundedAbstractCapability("capEF",null,null,null,null) )
    s.tasks = Set(t1,t2,t3)

    val x1 = WfGateway("x0",Array("scen1","scen2"))
    s.gateways += x1

    s.arcs += WfFlow(s.start,t1)
    s.arcs += WfFlow(t1,t2)
    s.arcs += WfFlow(t2,x1)
    s.arcs += WfFlow(x1,t3)

    //s.print_for_graphviz()
  }

  def testCompareSimpleSolution (): Unit = {
    var s1 = new Solution()
    var s2 = new Solution()

    val t1 = WfTask( GroundedAbstractCapability("capAB",null,null,null,null) )
    val t2 = WfTask( GroundedAbstractCapability("capBxCD",null,null,null,null) )
    val t3 = WfTask( GroundedAbstractCapability("capDF",null,null,null,null) )
    val t4 = WfTask( GroundedAbstractCapability("capEF",null,null,null,null) )
    s1.tasks = Set(t1,t2,t3)
    s2.tasks = Set(t1,t2,t4)

    val x0 = WfGateway("x0",Array("scen1","scen2"))
    s1.gateways += x0
    s1.arcs += WfFlow(s1.start,t1)
    s1.arcs += WfFlow(t1,t2)
    s1.arcs += WfFlow(t2,x0)
    s1.arcs += WfFlow(x0,t3,"scen1")
    s1.arcs += WfFlow(t3,s1.end)

    s2.gateways += x0
    s2.arcs += WfFlow(s2.start,t1)
    s2.arcs += WfFlow(t1,t2)
    s2.arcs += WfFlow(t2,x0)
    s2.arcs += WfFlow(x0,t4,"scen2")
    s2.arcs += WfFlow(t4,s1.end)

    //s1.print_for_graphviz()
    //s2.print_for_graphviz()

    //val (i1,i2) = Solution.compare_until_difference(s1,s2)

    //println(i1)
    //println(i2)

    val builder = new MultiSolutionBuilder
    val s_opt = Solution.merge_partial_solution_with_solution_path(s1,s2)
    //if (s_opt.isDefined)
    //  s_opt.get.print_for_graphviz()
  }

  def testCompareLoopedSolution (): Unit = {
    var s1 = new Solution()
    var s2 = new Solution()

    val t1 = WfTask( GroundedAbstractCapability("capAB",null,null,null,null) )
    val t2 = WfTask( GroundedAbstractCapability("capBxCD",null,null,null,null) )
    val t3 = WfTask( GroundedAbstractCapability("capDF",null,null,null,null) )

    val t4 = WfTask( GroundedAbstractCapability("capEF",null,null,null,null) )
    s1.tasks = Set(t1,t2,t3)
    s2.tasks = Set(t1,t2,t4)

    val x0 = WfGateway("x0",Array("scen1","scen2"))
    s1.gateways += x0
    s1.arcs += WfFlow(s1.start,t1)
    s1.arcs += WfFlow(t1,t2)
    s1.arcs += WfFlow(t2,x0)
    s1.arcs += WfFlow(x0,t3,"scen1")
    s1.arcs += WfFlow(t3,s1.end)

    s2.gateways += x0
    s2.arcs += WfFlow(s2.start,t1)
    s2.arcs += WfFlow(t1,t2)
    s2.arcs += WfFlow(t2,x0)
    s2.arcs += WfFlow(x0,t4,"scen2")
    s2.arcs += WfFlow(t4,t2)

    //s1.print_for_graphviz()
    //s2.print_for_graphviz()

    val builder = new MultiSolutionBuilder
    val s_opt = Solution.merge_partial_solution_with_solution_path(s1,s2)
    if (s_opt.isDefined) {
      val s = s_opt.get
      assert(s.check_completeness)
      //s.print_for_graphviz()
    }
  }

  def testCompareUncompleteSolution (): Unit = {
    var s1 = new Solution()
    var s2 = new Solution()

    val t1 = WfTask( GroundedAbstractCapability("capAB",null,null,null,null) )
    val t2 = WfTask( GroundedAbstractCapability("capBxCD",null,null,null,null) )
    val t3 = WfTask( GroundedAbstractCapability("capDF",null,null,null,null) )

    val t4 = WfTask( GroundedAbstractCapability("capEF",null,null,null,null) )
    s1.tasks = Set(t1,t2,t3)
    s2.tasks = Set(t1,t2,t4)

    val x0 = WfGateway("x0",Array("scen1","scen2","scen3"))
    s1.gateways += x0
    s1.arcs += WfFlow(s1.start,t1)
    s1.arcs += WfFlow(t1,t2)
    s1.arcs += WfFlow(t2,x0)
    s1.arcs += WfFlow(x0,t3,"scen1")
    s1.arcs += WfFlow(t3,s1.end)

    s2.gateways += x0
    s2.arcs += WfFlow(s2.start,t1)
    s2.arcs += WfFlow(t1,t2)
    s2.arcs += WfFlow(t2,x0)
    s2.arcs += WfFlow(x0,t4,"scen2")
    s2.arcs += WfFlow(t4,t2)

    val builder = new MultiSolutionBuilder
    val s_opt = Solution.merge_partial_solution_with_solution_path(s1,s2)
    if (s_opt.isDefined) {
      val s = s_opt.get
      assert(!s.check_completeness)
    }
  }

  def testCompareUncompleteSolution2 (): Unit = {
    var s1 = new Solution()
    var s2 = new Solution()

    val t1 = WfTask( GroundedAbstractCapability("capAB",null,null,null,null) )
    val t2 = WfTask( GroundedAbstractCapability("capBxCD",null,null,null,null) )
    val t3 = WfTask( GroundedAbstractCapability("capDF",null,null,null,null) )

    val t4 = WfTask( GroundedAbstractCapability("capEF",null,null,null,null) )
    val t5 = WfTask( GroundedAbstractCapability("capFE",null,null,null,null) )
    s1.tasks = Set(t1,t2,t3)
    s2.tasks = Set(t1,t2,t4,t5)

    val x0 = WfGateway("x0",Array("scen1","scen2"))
    s1.gateways += x0
    s1.arcs += WfFlow(s1.start,t1)
    s1.arcs += WfFlow(t1,t2)
    s1.arcs += WfFlow(t2,x0)
    s1.arcs += WfFlow(x0,t3,"scen1")
    s1.arcs += WfFlow(t3,s1.end)

    s2.gateways += x0
    s2.arcs += WfFlow(s2.start,t1)
    s2.arcs += WfFlow(t1,t2)
    s2.arcs += WfFlow(t2,x0)
    s2.arcs += WfFlow(x0,t4,"scen2")
    s2.arcs += WfFlow(t4,t5)
    s2.arcs += WfFlow(t5,t4)

    val builder = new MultiSolutionBuilder
    val s_opt = Solution.merge_partial_solution_with_solution_path(s1,s2)
    val s = s_opt.get
    assert(!s.check_soundness)
  }

  def testCompareUncompleteSolution3 (): Unit = {
    var s1 = new Solution()
    var s2 = new Solution()

    val t1 = WfTask( GroundedAbstractCapability("capAB",null,null,null,null) )
    val t2 = WfTask( GroundedAbstractCapability("capBxCD",null,null,null,null) )
    val t3 = WfTask( GroundedAbstractCapability("capDF",null,null,null,null) )

    val t4 = WfTask( GroundedAbstractCapability("capEF",null,null,null,null) )
    val t5 = WfTask( GroundedAbstractCapability("capFE",null,null,null,null) )
    s1.tasks = Set(t1,t2,t3)
    s2.tasks = Set(t1,t2,t4,t5)

    val x0 = WfGateway("x0",Array("scen1","scen2"))
    s1.gateways += x0
    s1.arcs += WfFlow(s1.start,t1)
    s1.arcs += WfFlow(t1,t2)
    s1.arcs += WfFlow(t2,x0)
    s1.arcs += WfFlow(x0,t3,"scen1")
    s1.arcs += WfFlow(t3,s1.end)

    s2.gateways += x0
    s2.arcs += WfFlow(s2.start,t1)
    s2.arcs += WfFlow(t1,t2)
    s2.arcs += WfFlow(t2,x0)
    s2.arcs += WfFlow(x0,t4,"scen2")
    s2.arcs += WfFlow(t4,t5)
    s2.arcs += WfFlow(t5,t2)

    val builder = new MultiSolutionBuilder
    val s_opt = Solution.merge_partial_solution_with_solution_path(s1,s2)
    assert(s_opt.isDefined)
  }

  def testCheckGatewayCompatibility (): Unit = {
    val tA = WfTask.dummy("A")
    val tB = WfTask.dummy("B")
    val tC = WfTask.dummy("C")
    val tD = WfTask.dummy("D")
    val tE = WfTask.dummy("E")
    val tF = WfTask.dummy("F")
    val tG = WfTask.dummy("G")

    val G1 = WfGateway("x0",Array("scen1","scen2"))

    var solution = new Solution()
    solution.tasks = Set(tA,tB,tC)
    solution.gateways += G1
    solution.arcs += WfFlow(solution.start,tA)
    solution.arcs += WfFlow(tA,tB)
    solution.arcs += WfFlow(tB,G1)
    solution.arcs += WfFlow(G1,tC,"scen1")
    //solution.arcs += WfFlow(G1,tD,"scen2")
    solution.arcs += WfFlow(tC,solution.end)
    //solution.arcs += WfFlow(tD,solution.end)


    var solution2 = new Solution()
    solution2.tasks = Set(tA,tB,tD,tC)
    solution2.gateways += G1
    solution2.arcs += WfFlow(solution2.start,tA)
    solution2.arcs += WfFlow(tA,tB)
    solution2.arcs += WfFlow(tB,G1)
    solution2.arcs += WfFlow(G1,tC,"scen1")
    solution2.arcs += WfFlow(G1,tD,"scen2")
    solution2.arcs += WfFlow(tC,solution2.end)
    solution2.arcs += WfFlow(tD,solution.end)

    println(Solution.check_gateway_compatibility(G1.options, solution.arcs_out_from(G1), solution2.arcs_out_from(G1) ))
  }

  def testCheckPath (): Unit = {
    val tA = WfTask.dummy("A")
    val tB = WfTask.dummy("B")
    val tC = WfTask.dummy("C")
    val tD = WfTask.dummy("D")
    val tE = WfTask.dummy("E")
    val tF = WfTask.dummy("F")
    val tG = WfTask.dummy("G")

    val G1 = WfGateway("x0", Array("scen1", "scen2"))

    var solution = new Solution()
    solution.tasks = Set(tA, tB, tC)
    solution.gateways += G1
    solution.arcs += WfFlow(solution.start, tA)
    solution.arcs += WfFlow(tA, tB)
    solution.arcs += WfFlow(tB, G1)
    solution.arcs += WfFlow(G1, tC, "scen1")
    //solution.arcs += WfFlow(G1,tD,"scen2")
    solution.arcs += WfFlow(tC, solution.end)
    //solution.arcs += WfFlow(tD,solution.end)


    val revpath = Solution.reverse_path_in_a_sequence(solution,solution.start,G1,List())
    val path = revpath.reverse
    for (i <- path)
      println(i)
  }

  def testCheckGatewayPath (): Unit = {
    val tA = WfTask.dummy("A")
    val tB = WfTask.dummy("B")
    val tC = WfTask.dummy("C")
    val tD = WfTask.dummy("D")
    val tE = WfTask.dummy("E")
    val tF = WfTask.dummy("F")
    val tG = WfTask.dummy("G")

    val G1 = WfGateway("x0",Array("scen1","scen2"))

    var solution = new Solution()
    solution.tasks = Set(tA,tB,tC)
    solution.gateways += G1

    solution.arcs += WfFlow(solution.start,tA)
    solution.arcs += WfFlow(tA,tB)
    solution.arcs += WfFlow(tB,G1)
    solution.arcs += WfFlow(G1,tC,"scen1")
    //solution.arcs += WfFlow(G1,tD,"scen2")
    solution.arcs += WfFlow(tC,solution.end)
    //solution.arcs += WfFlow(tD,solution.end)


    var solution2 = new Solution()
    solution2.tasks = Set(tA,tB,tD,tC)
    solution2.gateways += G1
    solution2.arcs += WfFlow(solution2.start,tA)
    solution2.arcs += WfFlow(tA,tB)
    solution2.arcs += WfFlow(tB,G1)
    solution2.arcs += WfFlow(G1,tC,"scen1")
    solution2.arcs += WfFlow(G1,tD,"scen2")
    solution2.arcs += WfFlow(tC,solution2.end)
    solution2.arcs += WfFlow(tD,solution2.end)

    val revpath = Solution.reverse_path_in_a_sequence(solution,solution.start,G1,List())
    val flag = Solution.check_if_reverse_path_is_contained(solution2,revpath)
    assert(flag==true)
  }


  def testSingleSolutionBuilder (): Unit = {
    var builder = new SingleSolutionBuilder

    val tA = WfTask.dummy("A")
    val tB = WfTask.dummy("B")
    val tC = WfTask.dummy("C")
    val tD = WfTask.dummy("D")
    val tE = WfTask.dummy("E")
    val tF = WfTask.dummy("F")
    val tG = WfTask.dummy("G")

    val G1 = WfGateway("x0",Array("scen1","scen2"))

    builder.solution.tasks = Set(tA,tB,tC,tD)
    builder.solution.gateways += G1

    builder.solution.arcs += WfFlow(builder.solution.start,tA)
    builder.solution.arcs += WfFlow(tA,tB)
    builder.solution.arcs += WfFlow(tB,G1)
    builder.solution.arcs += WfFlow(G1,tC,"scen1")
    builder.solution.arcs += WfFlow(G1,tD,"scen2")
    builder.solution.arcs += WfFlow(tC,builder.solution.end)
    builder.solution.arcs += WfFlow(tD,builder.solution.end)


    var ps1 = new Solution()
    ps1.tasks = Set(tA,tB,tF,tG)
    ps1.gateways += G1
    ps1.arcs += WfFlow(ps1.start,tA)
    ps1.arcs += WfFlow(tA,tB)
    ps1.arcs += WfFlow(tB,G1)
    ps1.arcs += WfFlow(G1,tF,"scen2")
    ps1.arcs += WfFlow(tF,tG)
    ps1.arcs += WfFlow(tG,ps1.end)

    builder.solution.blend(ps1)

    builder.solution.optimize.print_for_graphviz()
  }


  def testSingleSolutionBuilder2 (): Unit = {
    var builder = new SingleSolutionBuilder

    val tA = WfTask.dummy("A")
    val tB = WfTask.dummy("B")
    val tC = WfTask.dummy("C")
    val tD = WfTask.dummy("D")
    val tE = WfTask.dummy("E")
    val tF = WfTask.dummy("F")
    val tG = WfTask.dummy("G")

    val G1 = WfGateway("x0",Array("scen1","scen2"))

    builder.solution.tasks = Set(tA,tB,tC,tD)
    builder.solution.gateways += G1
    builder.solution.arcs += WfFlow(builder.solution.start,tA)
    builder.solution.arcs += WfFlow(tA,tB)
    builder.solution.arcs += WfFlow(tB,G1)
    builder.solution.arcs += WfFlow(G1,tC,"scen1")
    builder.solution.arcs += WfFlow(G1,tD,"scen2")
    builder.solution.arcs += WfFlow(tC,builder.solution.end)
    builder.solution.arcs += WfFlow(tD,builder.solution.end)


    var ps1 = new Solution()
    ps1.tasks = Set(tA,tB,tE)
    ps1.arcs += WfFlow(ps1.start,tA)
    ps1.arcs += WfFlow(tA,tB)
    ps1.arcs += WfFlow(tB,tE)
    ps1.arcs += WfFlow(tE,ps1.end)

    builder.solution.blend(ps1)
    builder.solution.optimize.print_for_graphviz()
  }

  def testSingleSolutionBuilder3 (): Unit = {
    var builder = new SingleSolutionBuilder

    val tA = WfTask.dummy("A")
    val tB = WfTask.dummy("B")
    val tC = WfTask.dummy("C")
    val tD = WfTask.dummy("D")
    val tE = WfTask.dummy("E")
    val tF = WfTask.dummy("F")
    val tG = WfTask.dummy("G")

    val G1 = WfGateway("x0",Array("scen1","scen2"))

    builder.solution.tasks = Set(tA,tB,tC,tD)
    builder.solution.gateways += G1
    builder.solution.arcs += WfFlow(builder.solution.start,tA)
    builder.solution.arcs += WfFlow(tA,tB)
    builder.solution.arcs += WfFlow(tB,G1)
    builder.solution.arcs += WfFlow(G1,tC,"scen1")
    builder.solution.arcs += WfFlow(G1,tD,"scen2")
    builder.solution.arcs += WfFlow(tC,builder.solution.end)
    builder.solution.arcs += WfFlow(tD,builder.solution.end)


    var ps1 = new Solution()
    ps1.tasks = Set(tA,tB,tA)
    ps1.arcs += WfFlow(ps1.start,tA)
    ps1.arcs += WfFlow(tA,tB)
    ps1.arcs += WfFlow(tB,tA)

    builder.solution.blend(ps1)
    builder.solution.optimize.print_for_graphviz()
  }
}
