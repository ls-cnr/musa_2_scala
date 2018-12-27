package org.icar.musa.scenarios.ids

import java.io.File

import org.icar.fol.{AtomTerm, GroundPredicate}
import org.icar.musa.actor_model.RequestNewSession
import org.icar.musa.context._
import org.icar.musa.main_entity.{EvolutionScenario, LTLGoalParser}
import org.icar.musa.specification.{ProxyCapability, StateMonitorCapability}

import scala.io.Source

/* monitor a new file is placed in the checkin folder */
class IDS_Proxy_FileMonitor(path : String) extends ProxyCapability {
  import scala.concurrent.duration._

  val delay = 1 second
  val folder = new File(path+"/tmp/checkin")
  var already_found = List[String]()

  println(folder.getAbsolutePath)

  def generate_new_request(doc_id : String) : RequestNewSession = {
    val d1 = new DataIn()
    d1.registerVariable("document_id",doc_id)
    val w1 = StateOfWorld.create(GroundPredicate("request", AtomTerm("id")),GroundPredicate("document", AtomTerm("id")))

    RequestNewSession(d1,w1)
  }

  override def run(): Unit = {
    while (folder != null) {
      //println("looking at dir "+folder.getName)
      val files: Array[File] = folder.listFiles()
      for (file <- files) {
        val name = file.getName
        if (name != ".DS_Store" && !already_found.contains(name)) {
          //println("found "+name)
          val request = generate_new_request(name)
          my_callback(request)

          already_found = name :: already_found

        }
      }
      Thread.sleep(delay.toMillis)
    }
  }
}


class DocumentReadyForWork_Monitor(path:String) extends StateMonitorCapability {
  val protocol_folder = path+"/protocol/"
  override val envs: List[String] = List("protocol")

  override def init: Unit = {}

  override def check_state(in: Measurables): EvolutionScenario = {
    var evo = List[EvoOperator]()

    val doc_id = in.getVariableValue("document_id").getOrElse(-1)

    if (doc_id != -1) {
      val file_in_protocol = new File(protocol_folder +doc_id)
      //val record_in_protocol = new File(protocol_folder +"protocol"+proto_number+".txt")
      if (file_in_protocol.exists()) {
        evo = List(remove("request"),add("to_work(id)"))
      }
    }

    EvolutionScenario(evo.toArray)
  }

  override def terminate: Unit = {}
}

class DocumentReadyForDecision_Monitor(path:String) extends StateMonitorCapability {
  val attach_folder = path+"/attachment/"
  override val envs: List[String] = List("attachment")

  override def init: Unit = {}

  override def check_state(in: Measurables): EvolutionScenario = {
    var evo = List[EvoOperator]()

    val doc_id = in.getVariableValue("document_id").getOrElse(-1)

    if (doc_id != -1) {
      val attach_in_attachment = new File(attach_folder + "attach_to_"+doc_id)
      //val record_in_protocol = new File(protocol_folder +"protocol"+proto_number+".txt")
      if (attach_in_attachment.exists()) {
        evo = List(remove("to_work(id)"),remove("to_revise(id)"),add("worked(id)"),add("attachment(id)"))
      }
    }

    EvolutionScenario(evo.toArray)
  }

  override def terminate: Unit = {}
}


class DocumentOutOfDecision_Monitor(path:String) extends StateMonitorCapability {
  val attach_folder = path+"/attachment/"
  val supervisor_folder = path+"/supervisor/"
  override val envs: List[String] = List("decision")

  val ACCEPTED : Int = 1
  val TOREVISE : Int = 2
  val REJECTED : Int = 3


  override def init: Unit = {}


  override def check_state(in: Measurables): EvolutionScenario = {
    var evo = List[EvoOperator]()

    val doc_id = in.getVariableValue("document_id").getOrElse(-1)

    if (doc_id != -1) {
      val attach_in_attachment = new File(attach_folder + "attach_to_"+doc_id)
      if (attach_in_attachment.exists()) {
        val supervisor_note = new File(supervisor_folder + "decision_for_"+doc_id+".txt")
        if (supervisor_note.exists()) {
          val decision : Int = check_note_content(supervisor_note)
          decision match {
            case ACCEPTED => //ACCEPT
              println("accepted")
              evo = List(remove("worked(id)"),add("accepted(id)"))
              supervisor_note.renameTo(new File(supervisor_folder + "processed_decision_"+doc_id+".txt"))

            case TOREVISE =>
              println("to be revised")
              evo = List(remove("worked(id)"),add("to_revise(id)"))
              supervisor_note.renameTo(new File(supervisor_folder + "processed_decision_"+doc_id+".txt"))

            case REJECTED =>
              println("rejected")
              evo = List(remove("worked(id)"),add("rejected(id)"))
              supervisor_note.renameTo(new File(supervisor_folder + "processed_decision_"+doc_id+".txt"))

            case _ =>
              //println("not recognized")
          }
        }



      }
    }

    EvolutionScenario(evo.toArray)
  }

  private def check_note_content(supervisor_note: File): Int = {
    var ret : Int = -1

    val fileContent = Source.fromFile(supervisor_note).getLines.mkString.trim
    //println("content: "+fileContent)
    if (fileContent=="ACCEPTED")
      ret = ACCEPTED
    else if (fileContent=="REVISE")
      ret = TOREVISE
    else if (fileContent=="REJECTED")
      ret = REJECTED

    ret
  }

  override def terminate: Unit = {}
}


class DocumentNotified_Monitor(path:String) extends StateMonitorCapability {
  val notification_folder = path+"/refused/"
  override val envs: List[String] = List("notification")

  override def init: Unit = {}

  override def check_state(in: Measurables): EvolutionScenario = {
    var evo = List[EvoOperator]()

    val doc_id = in.getVariableValue("document_id").getOrElse(-1)

    if (doc_id != -1) {
      val doc_in_notification = new File(notification_folder +doc_id)
      if (doc_in_notification.exists()) {
        evo = List(add("notify_rejection(id)"))
      }
    }

    EvolutionScenario(evo.toArray)
  }

  override def terminate: Unit = {}
}


/*
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
*/