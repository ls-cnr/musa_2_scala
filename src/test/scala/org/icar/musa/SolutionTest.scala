package org.icar.musa

import junit.framework.TestCase
import org.icar.musa.pmr._
import org.icar.musa.spec.GroundedAbstractCapability

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

    val (i1,i2) = Solution.compare_until_difference(s1,s2)

    //println(i1)
    //println(i2)

    val builder = new MultiSolutionBuilder
    val s_opt = builder.merge_xor_sequences(s1,s2)
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
    val s_opt = builder.merge_xor_sequences(s1,s2)
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
    val s_opt = builder.merge_xor_sequences(s1,s2)
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
    val s_opt = builder.merge_xor_sequences(s1,s2)
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
    val s_opt = builder.merge_xor_sequences(s1,s2)
    assert(s_opt.isDefined)
  }
}
