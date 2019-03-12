package org.icar.musa.scenarios.ids

import java.io.{File, FileOutputStream, PrintWriter}

import org.icar.fol._
import org.icar.musa.context._
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
      conc_repo += new ProtocolFactory(abstract1.get,path+"/ids_data/tmp")
    val abstract2 = recover_abstract("work_document",repository)
    if (abstract2.isDefined)
      conc_repo += new WorkFactory(abstract2.get,this.assumption,path+"/ids_data/tmp")
    val abstract3 = recover_abstract("supervise_attachment",repository)
    if (abstract3.isDefined)
      conc_repo += new SuperviseFactory(abstract3.get,path+"/ids_data/tmp")
    val abstract4 = recover_abstract("notify_rejection",repository)
    if (abstract4.isDefined)
      conc_repo += new NotifyFactory(abstract4.get,path+"/ids_data/tmp")


    conc_repo.toArray
  }

  override def monitors : List[StateMonitorCapability] = List(
    new DocumentReadyForWork_Monitor(path+"/ids_data/tmp"),
    new DocumentReadyForDecision_Monitor(path+"/ids_data/tmp"),
    new DocumentOutOfDecision_Monitor(path+"/ids_data/tmp"),
    new DocumentNotified_Monitor(path+"/ids_data/tmp")

  )

  override def grounder_type: GrounderProperty = EndToEnd()
  override def solution_type: SolutionProperty = EarlyDecisionWorkflow()
  override def session_type : SessionProperty = MultiSession()


  override def proxy_strategy: Option[ProxyCapability] = Some(new IDS_Proxy_FileMonitor(path+"/ids_data"))// Some(new IDS_Proxy_Cyclic)

  override def active = false
}














