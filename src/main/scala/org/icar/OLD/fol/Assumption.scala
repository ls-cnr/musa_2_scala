package org.icar.fol

import java.util

import net.sf.tweety.lp.asp.syntax.{Rule => TweetyRule}

case class Assumption(rule: TweetyRule) {

  override def toString: String = rule.toString

}

object Assumption {
  def apply(ruleString : String): Assumption = {
    var ass : Assumption = null

    this.synchronized{
      //val rule = ASPParser.parseRule(ruleString)
      val rule = Entail.rule_for_asl(ruleString)
      ass = new Assumption(rule)
    }

    ass
  }
}

case class AssumptionSet(rules : Assumption*) {
  def as_list : util.ArrayList[TweetyRule] = {
    val list = new util.ArrayList[TweetyRule]()
    for (a <- rules)
      list.add(a.rule)
    list
  }
}

