package org.icar.fol

//import java.util._
import java.nio.file.{Files, Paths}

import net.sf.tweety.logics.fol.semantics.HerbrandInterpretation
import net.sf.tweety.lp.asp.solver.DLV
import net.sf.tweety.logics.translators.aspfol.AspFolTranslator
import net.sf.tweety.logics.fol.syntax.FOLAtom
import net.sf.tweety.lp.asp.syntax.Program
import org.icar.musa.context.StateOfWorld
import org.icar.musa.spec.AbstractCapability

object Entail {

  val PATH_DLV: String = getPath
  val solver = new DLV(PATH_DLV)
  val tx = new AspFolTranslator()

  private def getPath:String = {
    var path = "/Users/luca/Workspaces/workspace-neon/musa_2_scala_agents/ext"//System.getProperty("dlv_install")

    //var path=""
    val sys = System.getProperty("os.name")
    if (sys.startsWith("Windows") )
      path += "/dlv.mingw.exe"
    else
      path += "/dlv.i386-apple-darwin.bin"

    path
  }
  
  def condition(w : StateOfWorld, assertionset: AssumptionSet, c : FOLCondition) : Boolean = {
    import net.sf.tweety.lp.asp.syntax.Rule
    import net.sf.tweety.lp.asp.syntax.Program
    import net.sf.tweety.lp.asp.parser.ASPParser
    import net.sf.tweety.logics.fol.semantics.HerbrandInterpretation
    
    var reply = false

    val base = new Program(assertionset.as_list)

    for (s <- w.statements)
      base.addFact(s.rule_for_asl)

    val response = solver.computeModels(base, 10000)

    if (response != null) {
			val as = response.get(0)
			val interpr = new HerbrandInterpretation()


			val it = as.iterator()
			while (it.hasNext) {
				val f = tx.toFOL(it.next())
				interpr.add(f.asInstanceOf[FOLAtom])
			}

			reply = interpr satisfies TweetyFormula.fromCond(c)

		}
    
    reply
  }

  def condition_map(w : StateOfWorld, assertionset: AssumptionSet, cond_map: Map[String, GroundPredicate]) : Map[String, Boolean] = {
    import net.sf.tweety.lp.asp.syntax.Rule
    import net.sf.tweety.lp.asp.syntax.Program
    import net.sf.tweety.lp.asp.parser.ASPParser
    import net.sf.tweety.logics.fol.semantics.HerbrandInterpretation

    var reply = Map[String, Boolean]()

    val base = new Program(assertionset.as_list)

    for (s <- w.statements)
      base.addFact(s.rule_for_asl)

    val response = solver.computeModels(base, 10000)

    if (response != null) {
      val as = response.get(0)
      val interpr = new HerbrandInterpretation()


      val it = as.iterator()
      while (it.hasNext) {
        val f = tx.toFOL(it.next())
        interpr.add(f.asInstanceOf[FOLAtom])
      }

      for (name : String <- cond_map.keySet) {
        val c = cond_map(name)
        val tweety = TweetyFormula.fromCond(FOLCondition(GroundLiteral(c)))
        //println("checking: "+tweety)
        val b : Boolean = interpr.satisfies( tweety )
        reply = reply ++ Map(name -> b)
      }

    }

    reply
  }

  def capabilities(w : StateOfWorld, assertionset: AssumptionSet, capabilities: Array[AbstractCapability]) : Map[String, Boolean] = {
    var reply = Map[String, Boolean]()

    val base = new Program(assertionset.as_list)

    for (s <- w.statements)
      base.addFact(s.rule_for_asl)

    val response = solver.computeModels(base, 10000)

    if (response != null) {
      val as = response.get(0)
      val interpr = new HerbrandInterpretation()


      val it = as.iterator()
      while (it.hasNext) {
        val f = tx.toFOL(it.next())
        interpr.add(f.asInstanceOf[FOLAtom])
      }

      for (c <- capabilities) {
        val pre = FOLCondition(c.pre.formula)
        val post = FOLCondition(Negation(c.post.formula))
        val tweety_pre = TweetyFormula.fromCond(pre)
        val tweety_post = TweetyFormula.fromCond(post)
        val pre_is_true : Boolean = interpr.satisfies( tweety_pre )
        val post_is_false : Boolean = interpr.satisfies( tweety_post )
        val cap_cond = pre_is_true&post_is_false
        reply = reply ++ Map(c.name -> cap_cond)

      }

    }

    reply
  }

}