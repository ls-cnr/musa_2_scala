package org.icar.musa.actor

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import org.icar.musa.pmr.SingleGoalProblemSpecification
import org.icar.musa.scenarios.{PRINEntertainmentScenario, PRINWakeUpScenario}


class DomainActor(musa_db : DBInfo, domain_id : DomainInfo) extends Actor with ActorLogging {
  val spec : SingleGoalProblemSpecification = load_specifications(musa_db,domain_id)

  init


  private def init : Unit = {
    log.info("ready for (id="+domain_id+")")

    on_demand_strategy
  }

  private def on_demand_strategy : Unit = {
    val context_props = Props.create(classOf[ContextActor],musa_db,domain_id)
    val context_actor : ActorRef = context.actorOf(context_props, "context")

    val self_conf_props = Props.create(classOf[SelfConfActor],spec,musa_db,domain_id)
    val self_conf_actor : ActorRef = context.actorOf(self_conf_props, "self-conf")

    val orchestrator_props = Props.create(classOf[OrchestratorActor],spec,self_conf_actor,musa_db,domain_id)
    val orchestrator_actor : ActorRef = context.actorOf(orchestrator_props, "orchestrator")
  }


  override def receive: Receive = {
    case "init" ⇒

  }






  private def load_specifications(musa_db: DBInfo, domain_id: DomainInfo): SingleGoalProblemSpecification = {

    val sc = new PRINWakeUpScenario //PRINEntertainmentScenario
    val assumption = sc.assumption_set
    val goals = sc.goal_specification
    val quality = sc.quality_asset

    SingleGoalProblemSpecification(assumption,goals,quality)
  }


/* PER IL MOMENTO NON USO QUESTO METODO
  private def check_single_solution(musa_db:DBInfo, domain_id: Int) : Boolean = {
    import java.sql.{Connection, DriverManager}
    var flag = false

    try {
      Class.forName(musa_db.driver)
      var connection:Connection = DriverManager.getConnection(musa_db.url, musa_db.user, musa_db.psw)
      val statement = connection.createStatement
      val where_st = "WHERE idDomain='"+domain_id+"' AND name='solution'"
      val rs: ResultSet = statement.executeQuery("SELECT value FROM domain_configuration "+where_st)

      if (rs.first) {
        val value : String = rs.getString("value")
        if (value=="single")
          flag=true
      }

      connection.close
    } catch {
      case e: Exception => e.printStackTrace
    }

    flag
  }
*/


}
