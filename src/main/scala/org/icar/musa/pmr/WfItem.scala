package org.icar.musa.pmr

import org.icar.fol.{AlwaysTrue, FOLCondition}
import org.icar.musa.main_entity.{AbstractCapability, GroundedAbstractCapability}

abstract class WfItem
abstract class WfEvent extends WfItem
case class WfStartEvent() extends WfEvent {
  override def hashCode(): Int = "start".hashCode()
}
case class WfEndEvent() extends WfEvent {
  override def hashCode(): Int = "end".hashCode()
}
case class WfTask(cap : AbstractCapability) extends WfItem {
  override def hashCode(): Int = cap.name.hashCode()
}
object WfTask {
  def dummy(cap : String) = WfTask(GroundedAbstractCapability(cap,null,null,null,null))
}
case class WfGateway(name : String, options: Array[String]) extends WfItem {
  override def hashCode(): Int = name.hashCode()
}
case class WfFlow(from: WfItem, to: WfItem, decision : String ="", condition: FOLCondition=FOLCondition(AlwaysTrue())) {
  override def hashCode(): Int = from.hashCode()+to.hashCode()+decision.hashCode
}
