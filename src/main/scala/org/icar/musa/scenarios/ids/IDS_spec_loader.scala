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
    new WorkerDoneMonitor(path+"/ids_data/tmp"),
    new SupervisorDoneMonitor(path+"/ids_data/tmp")
  )

  override def grounder_type: GrounderProperty = EndToEnd()
  override def solution_type: SolutionProperty = EarlyDecisionWorkflow()
  override def session_type : SessionProperty = MultiSession()


  override def proxy_strategy: Option[ProxyCapability] = Some(new IDS_Proxy_FileMonitor(path+"/ids_data"))// Some(new IDS_Proxy_Cyclic)

  override def active = true
}








class ProtocolFactory(abs_cap : GroundedAbstractCapability, path : String) extends ConcreteCapabilityFactory {
  override def getAbstractName: String = "protocol_request"
  override def getInstance : ConcreteCapability = new Protocol1(abs_cap,path)
}
class Protocol1(abs_cap : GroundedAbstractCapability, path : String) extends ConcreteCapability("protocol_1",abs_cap) {
  val infolder = "/checkin/"
  val outfolder = "/protocol/"
  var proto_number = 0

  override def init: Unit = { println("init Protocol") }
  override def pre_start: Unit = { println("prestart Protocol")}
  override def execute(w : StateOfWorld,in:Measurables): Unit = {
    println("executing Protocol")
    val id = in.getVariableValue("document_id").getOrElse(-1)
    if (id != -1) {
      println("document id = "+id)

      val file = new File(path+infolder+id)
      file.renameTo(new File(path+outfolder+id))

      proto_number += 1
      val proto = new File(path+outfolder+"protocol"+proto_number+".txt")
      val pw = new PrintWriter(proto)
      pw.write("document: id="+id)
      pw.close()

      println("executed Protocol")
    }

  }
  override def post_end: Unit = { println("Protocol has been successfull") }
  override def compensate: Unit = { println("compensate Protocol") }
  override def terminate: Unit = { println("delete Protocol") }
}

class WorkFactory(abs_cap : GroundedAbstractCapability, assumptions : AssumptionSet, path : String) extends ConcreteCapabilityFactory {
  override def getAbstractName: String = "work_document"
  override def getInstance : ConcreteCapability = new Work1(abs_cap,assumptions,path)
}
class Work1(abs_cap : GroundedAbstractCapability, assumptions : AssumptionSet, path : String) extends ConcreteCapability("work_1",abs_cap) {
  val infolder = "/protocol/"
  val myfolder = "/worker1/"
  val supfolder = "/supervisor/"
  var id : Any = "None"

  override def init: Unit = { println("init Work") }
  override def pre_start: Unit = {
    println("prestart Work")
  }
  override def execute(w : StateOfWorld,in:Measurables): Unit = {
    println("executing Work")
    id = in.getVariableValue("document_id").getOrElse("None")
    if (id != "None") {
      if (Entail.condition(w,assumptions,FOLCondition(Literal(Predicate("to_work", AtomTerm("id")))))) {
          val file = new File(path + infolder + id)
          file.renameTo(new File(path + myfolder + id))
      } else if (Entail.condition(w,assumptions,FOLCondition(Literal(Predicate("to_revise", AtomTerm("id")))))) {
        val file = new File(path + supfolder + id)
        file.renameTo(new File(path + myfolder + id))
      }

    }
  }
  override def get_simulated_scenario = None
  override def post_end: Unit = {
    println("Work has been successfull")
    if (id != "None") {
      val file1 = new File(path + myfolder + id)
      file1.renameTo(new File(path + supfolder + id))
      val file2 = new File(path + myfolder + "attach_to_"+ id)
      file2.renameTo(new File(path + supfolder + "attach_to_"+id))
    }
  }
  override def compensate: Unit = { println("compensate Work") }
  override def terminate: Unit = { println("delete Work") }
}

class SuperviseFactory(abs_cap : GroundedAbstractCapability, path : String) extends ConcreteCapabilityFactory {
  override def getAbstractName: String = "supervise_attachment"
  override def getInstance : ConcreteCapability = new Supervise1(abs_cap,path)
}
class Supervise1(abs_cap : GroundedAbstractCapability, path : String) extends ConcreteCapability("supervise_1",abs_cap) {
  override def init: Unit = { println("init Supervise") }
  override def pre_start: Unit = { println("prestart Supervise")}
  override def execute(w : StateOfWorld,in:Measurables): Unit = { println("executing Supervise") }
  override def get_simulated_scenario = None
  override def post_end: Unit = { println("Supervise has been successfull") }
  override def compensate: Unit = { println("compensate Supervise") }
  override def terminate: Unit = { println("delete Supervise") }
}

class NotifyFactory(abs_cap : GroundedAbstractCapability, path : String) extends ConcreteCapabilityFactory {
  override def getAbstractName: String = "notify_rejection"
  override def getInstance : ConcreteCapability = new Notify1(abs_cap,path)
}
class Notify1(abs_cap : GroundedAbstractCapability, path : String) extends ConcreteCapability("notify_1",abs_cap) {
  override def init: Unit = { println("init Notify") }
  override def pre_start: Unit = { println("prestart Notify")}
  override def execute(w : StateOfWorld,in:Measurables): Unit = { println("executing Notify") }
  override def get_simulated_scenario = None
  override def post_end: Unit = { println("Notify has been successfull") }
  override def compensate: Unit = { println("compensate Notify") }
  override def terminate: Unit = { println("delete Notify") }
}





class WorkerDoneMonitor(path:String) extends StateMonitorCapability {
  val folder = "/worker1/"
  override val envs: List[String] = List("attach")

  override def init: Unit = {}

  override def check_state(in:Measurables) : EvolutionScenario = {
    var evo = List[EvoOperator]()

    val id = in.getVariableValue("document_id").getOrElse(-1)
    if (id != -1) {
      val file_to_check = new File(path + folder + "attach_to_"+id)
      if (file_to_check.exists()) {
        //remove to_work(id), remove to_revise(id) , add worked(id), add attachment(id)
        val parser = new LTLGoalParser

        val rmv1 = RemoveEvoOperator(parser.parseAll(parser.predicate,"to_work(id)").get)
        val rmv2 = RemoveEvoOperator(parser.parseAll(parser.predicate,"to_revise(id)").get)
        val add1 = AddEvoOperator(parser.parseAll(parser.predicate,"worked(id)").get)
        val add2 = AddEvoOperator(parser.parseAll(parser.predicate,"attachment(id)").get)
        evo = List(rmv1,rmv2,add1,add2)
      }
    }

    EvolutionScenario(evo.toArray)
  }

  override def terminate: Unit = {}
}

class SupervisorDoneMonitor(path:String) extends StateMonitorCapability {
  val folder = "/supervisor/"
  override val envs: List[String] = List("decision")

  override def init: Unit = {}

  override def check_state(in:Measurables) : EvolutionScenario = {
    var evo = List[EvoOperator]()

    val id = in.getVariableValue("document_id").getOrElse("None")
    if (id != "None") {
      val check_if_accepted = new File(path + folder + "ACCEPTED_"+id)
      if (check_if_accepted.exists()) {
        //remove worked(id), add accepted(id)
        val parser = new LTLGoalParser

        val rmv1 = RemoveEvoOperator(parser.parseAll(parser.predicate,"worked(id)").get)
        val add1 = AddEvoOperator(parser.parseAll(parser.predicate,"accepted(id)").get)
        evo = List(rmv1,add1)
      }
      val check_if_refused = new File(path + folder + "REFUSED_"+id)
      if (check_if_refused.exists()) {
        //remove worked(id), add rejected(id)
        val parser = new LTLGoalParser

        val rmv1 = RemoveEvoOperator(parser.parseAll(parser.predicate,"worked(id)").get)
        val add1 = AddEvoOperator(parser.parseAll(parser.predicate,"rejected(id)").get)
        evo = List(rmv1,add1)
      }
      val check_if_revision = new File(path + folder + "TO_REVISE_"+id)
      if (check_if_revision.exists()) {
        //remove worked(id), add to_revise(id)
        val parser = new LTLGoalParser

        val rmv1 = RemoveEvoOperator(parser.parseAll(parser.predicate,"worked(id)").get)
        val add1 = AddEvoOperator(parser.parseAll(parser.predicate,"to_revise(id)").get)
        evo = List(rmv1,add1)
      }

    }

    EvolutionScenario(evo.toArray)
  }

  override def terminate: Unit = {}
}

