package org.icar.pmr_solver

import java.util

import org.icar.fol.Assumption
import net.sf.tweety.lp.asp.syntax.{Rule => TweetyRule}


case class Domain (val types : Array[DomainType], val atoms : Array[DomainConst], val axioms : Array[Assumption]) {

  def axioms_as_rulelist: util.ArrayList[TweetyRule] = {
    val list = new util.ArrayList[TweetyRule]()
    for (a <- axioms)
      list.add(a.rule)
    list
  }

}


abstract class DomainType(name : String)
case class NumericDomainType(name : String, min : Integer, max : Integer) extends DomainType(name)
case class EnumerativeDomainType(name : String, range : Array[String])



case class DomainConst(name : String)

