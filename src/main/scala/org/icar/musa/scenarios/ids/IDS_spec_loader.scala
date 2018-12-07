package org.icar.musa.scenarios.ids

import org.icar.musa.context.{Measurables, StateOfWorld}
import org.icar.musa.main_entity._
import org.icar.musa.pmr.{EmptyQualityAsset, QualityAsset}
import org.icar.musa.specification._

import scala.collection.mutable.ArrayBuffer

class IDS_spec_loader (path: String) extends SpecificationLoader {
  override def domains: Array[DomainLoader] = {
    Array(new IDS_domain_loader(path))
  }
}



class IDS_domain_loader(path: String) extends DefaultDomainLoader(
                                                    path+"/ids_data/IDS_assumptions.rules",
                                                    path+"/ids_data/IDS_goal.ltl",
                                                    path+"/ids_data/IDS_capabilities.cap"
                                                  ) {
  override def name: String = "ids"

  override def initial_state: StateOfWorld = StateOfWorld.empty

  override def quality_asset: QualityAsset = new EmptyQualityAsset(assumption)

  override def concrete_repository: Array[ConcreteCapabilityFactory] = {
    var conc_repo: ArrayBuffer[ConcreteCapabilityFactory] = ArrayBuffer()
    lazy val repository = abstract_repository
    /* populate */

    val abstract1 = recover_abstract("protocol_request",repository)
    if (abstract1.isDefined)
      conc_repo += new ProtocolFactory(abstract1.get)
    val abstract2 = recover_abstract("work_document",repository)
    if (abstract2.isDefined)
      conc_repo += new WorkFactory(abstract2.get)
    val abstract3 = recover_abstract("supervise_attachment",repository)
    if (abstract3.isDefined)
      conc_repo += new SuperviseFactory(abstract3.get)
    val abstract4 = recover_abstract("notify_rejection",repository)
    if (abstract4.isDefined)
      conc_repo += new NotifyFactory(abstract4.get)


    conc_repo.toArray
  }

  override def grounder_type: GrounderProperty = EndToEnd()
  override def solution_type: SolutionProperty = AllInOneWorkflow()
  override def session_type : SessionProperty = MultiSession()

  override def active = false
}








class ProtocolFactory(abs_cap : GroundedAbstractCapability) extends ConcreteCapabilityFactory {
  override def getAbstractName: String = "protocol_request"
  override def getInstance : ConcreteCapability = new Protocol1(abs_cap)
}
class Protocol1(abs_cap : GroundedAbstractCapability) extends ConcreteCapability("protocol_1",abs_cap) {
  override def init: Unit = { println("init Protocol") }
  override def pre_start: Unit = { println("prestart Protocol")}
  override def execute(in:Measurables): Unit = {
    println("executing Protocol")
    val id = in.getVariableValue("document_id").getOrElse(-1)
    println("document id = "+id)
    println("executed Protocol")

  }
  override def post_end: Unit = { println("Protocol has been successfull") }
  override def compensate: Unit = { println("compensate Protocol") }
  override def terminate: Unit = { println("delete Protocol") }
}

class WorkFactory(abs_cap : GroundedAbstractCapability) extends ConcreteCapabilityFactory {
  override def getAbstractName: String = "work_document"
  override def getInstance : ConcreteCapability = new Work1(abs_cap)
}
class Work1(abs_cap : GroundedAbstractCapability) extends ConcreteCapability("work_1",abs_cap) {
  override def init: Unit = { println("init Work") }
  override def pre_start: Unit = { println("prestart Work")}
  override def execute(in:Measurables): Unit = { println("executing Work") }
  override def post_end: Unit = { println("Work has been successfull") }
  override def compensate: Unit = { println("compensate Work") }
  override def terminate: Unit = { println("delete Work") }
}

class SuperviseFactory(abs_cap : GroundedAbstractCapability) extends ConcreteCapabilityFactory {
  override def getAbstractName: String = "supervise_attachment"
  override def getInstance : ConcreteCapability = new Work1(abs_cap)
}
class Supervise1(abs_cap : GroundedAbstractCapability) extends ConcreteCapability("supervise_1",abs_cap) {
  override def init: Unit = { println("init Supervise") }
  override def pre_start: Unit = { println("prestart Supervise")}
  override def execute(in:Measurables): Unit = { println("executing Supervise") }
  override def post_end: Unit = { println("Supervise has been successfull") }
  override def compensate: Unit = { println("compensate Supervise") }
  override def terminate: Unit = { println("delete Supervise") }
}

class NotifyFactory(abs_cap : GroundedAbstractCapability) extends ConcreteCapabilityFactory {
  override def getAbstractName: String = "notify_rejection"
  override def getInstance : ConcreteCapability = new Work1(abs_cap)
}
class Notify1(abs_cap : GroundedAbstractCapability) extends ConcreteCapability("notify_1",abs_cap) {
  override def init: Unit = { println("init Notify") }
  override def pre_start: Unit = { println("prestart Notify")}
  override def execute(in:Measurables): Unit = { println("executing Notify") }
  override def post_end: Unit = { println("Notify has been successfull") }
  override def compensate: Unit = { println("compensate Notify") }
  override def terminate: Unit = { println("delete Notify") }
}