package org.icar.musa.scenarios.ids

import java.io.{File, FileWriter, PrintWriter}

import org.icar.fol._
import org.icar.musa.context.{Measurables, StateOfWorld}
import org.icar.musa.main_entity.{ConcreteCapability, ConcreteCapabilityFactory, GroundedAbstractCapability}

class ProtocolFactory(abs_cap : GroundedAbstractCapability, path : String) extends ConcreteCapabilityFactory {
  override def getAbstractName: String = "protocol_request"
  override def getInstance : ConcreteCapability = new Protocol_Capability(abs_cap,path)
}
class Protocol_Capability(abs_cap : GroundedAbstractCapability, path : String) extends ConcreteCapability("protocol_1",abs_cap) {
  val infolder = "/checkin/"
  val outfolder = "/protocol/"

  override def init: Unit = { println("init Protocol") }
  override def pre_start: Unit = { println("prestart Protocol")}
  override def execute(w : StateOfWorld,in:Measurables): Unit = {
    println("executing Protocol")
    val id = in.getVariableValue("document_id").getOrElse(-1)
    if (id != -1) {
      println("document id = "+id)

      val file = new File(path+infolder+id)
      file.renameTo(new File(path+outfolder+id))

      val proto = new File(path+outfolder+"protocol.txt")
      val pw = new FileWriter(proto,true)
      pw.write("document: id="+id+"\n")
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
  override def getInstance : ConcreteCapability = new Worker_Capability(abs_cap,assumptions,path)
}
class Worker_Capability(abs_cap : GroundedAbstractCapability, assumptions : AssumptionSet, path : String) extends ConcreteCapability("work_1",abs_cap) {
  val infolder = "/attachment/"
  val myfolder = "/worker1/"
  //val supfolder = "/supervisor/"
  //var id : Any = "None"

  override def init: Unit = { println("init Work") }
  override def pre_start: Unit = {
    println("prestart Work")
  }
  override def execute(w : StateOfWorld,in:Measurables): Unit = {
    println("executing Work")
    val id = in.getVariableValue("document_id").getOrElse("None")
    if (id != "None") {
      val todo = new File(path + myfolder + "todo_"+id+".txt")
      if (!todo.exists())
        todo.createNewFile()

      if (Entail.condition(w,assumptions,FOLCondition(Literal(Predicate("to_work", AtomTerm("id")))))) {
        println("first time")
        val pw = new FileWriter(todo,true)
        pw.write("job: id="+id+"\n")
        pw.close()
        //val file = new File(path + infolder + id)
        //file.renameTo(new File(path + myfolder + id))
      } else if (Entail.condition(w,assumptions,FOLCondition(Literal(Predicate("to_revise", AtomTerm("id")))))) {
        println("other times")

        val oldattach = new File(path + infolder + "old_attach_to_"+id)
        val attach = new File(path + infolder + "attach_to_"+id)
        if (oldattach.exists())
          oldattach.delete()

        attach.renameTo(oldattach)

        val pw = new FileWriter(todo,true)
        pw.write("revision: id="+id+"\n")
        pw.close()
        //val file = new File(path + supfolder + id)
        //file.renameTo(new File(path + myfolder + id))
      } else {
        println("something wrong")
      }

    }
  }
  override def get_simulated_scenario: None.type = None
  override def post_end: Unit = {
    println("Work has been successfull")
    /*if (id != "None") {
      val file1 = new File(path + myfolder + id)
      file1.renameTo(new File(path + supfolder + id))
      val file2 = new File(path + myfolder + "attach_to_"+ id)
      file2.renameTo(new File(path + supfolder + "attach_to_"+id))
    }*/
  }
  override def compensate: Unit = { println("compensate Work") }
  override def terminate: Unit = { println("delete Work") }
}

class SuperviseFactory(abs_cap : GroundedAbstractCapability, path : String) extends ConcreteCapabilityFactory {
  override def getAbstractName: String = "supervise_attachment"
  override def getInstance : ConcreteCapability = new Supervise_Capability(abs_cap,path)
}
class Supervise_Capability(abs_cap : GroundedAbstractCapability, path : String) extends ConcreteCapability("supervise_1",abs_cap) {
  val infolder = "/protocol/"
  val myfolder = "/supervisor/"

  override def init: Unit = { println("init Supervise") }
  override def pre_start: Unit = { println("prestart Supervise")}
  override def execute(w : StateOfWorld,in:Measurables): Unit = {
    println("executing Supervise")
    val id = in.getVariableValue("document_id").getOrElse("None")
    if (id != "None") {
      val todo = new File(path + myfolder + "decision_for_"+id+".txt")
      todo.createNewFile()
    }

  }
  override def get_simulated_scenario: None.type = None
  override def post_end: Unit = { println("Supervise has been successfull") }
  override def compensate: Unit = { println("compensate Supervise") }
  override def terminate: Unit = { println("delete Supervise") }
}

class NotifyFactory(abs_cap : GroundedAbstractCapability, path : String) extends ConcreteCapabilityFactory {
  override def getAbstractName: String = "notify_rejection"
  override def getInstance : ConcreteCapability = new Notify_Capability(abs_cap,path)
}

class Notify_Capability(abs_cap : GroundedAbstractCapability, path : String) extends ConcreteCapability("notify_1",abs_cap) {
  val infolder = "/protocol/"
  val outfolder = "/refused/"

  override def init: Unit = { println("init Notify") }
  override def pre_start: Unit = { println("prestart Notify")}
  override def execute(w : StateOfWorld,in:Measurables): Unit = {
    println("executing Notify")
    val id = in.getVariableValue("document_id").getOrElse(-1)
    if (id != -1) {
      val file = new File(path + infolder + id)
      file.renameTo(new File(path + outfolder + id))
    }
  }
  override def get_simulated_scenario: None.type = None
  override def post_end: Unit = { println("Notify has been successfull") }
  override def compensate: Unit = { println("compensate Notify") }
  override def terminate: Unit = { println("delete Notify") }
}

