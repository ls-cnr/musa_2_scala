package org.icar.actor_model.core

import akka.actor.{Actor, ActorLogging}
import javax.swing.{JFrame, JTabbedPane, JTextArea}
import org.icar.actor_model.core.MUSAActor.ui

import scala.concurrent.ExecutionContextExecutor
import scala.swing.{Dimension, TextArea}

trait MUSAActor extends Actor {
	val system = context.system
	implicit val executionContext: ExecutionContextExecutor = system.dispatcher

	var roles : List[Receive] = List(not_understood)
	protected def registerRole(receive: Receive) {
		roles = receive :: roles
	}

	private def not_understood : Receive = {
		case _ => NotUnderstood
	}

	def receive: Receive = roles reduce {_ orElse _}

	val my_log_area=MUSAActor.register_actor(self.path.name)

	def mylog(string:String) : Unit = {
		//println(string)

		my_log_area.append(string+"\n")
	}
}

object MUSAActor {
	val ui = new MUSAActorUI("MUSA GUI")
	ui.frame.setVisible(true)

	def register_actor(name:String): JTextArea = {
		val my_text_area = new JTextArea()
		ui.tabbed.add(name,my_text_area)
		my_text_area
	}

}


class MUSAActorUI(title:String) {
	val frame = new JFrame(title)
	val tabbed = new JTabbedPane()
	frame.getContentPane.add(tabbed)
	frame.setPreferredSize(new Dimension(600, 500))
	frame.pack
}




