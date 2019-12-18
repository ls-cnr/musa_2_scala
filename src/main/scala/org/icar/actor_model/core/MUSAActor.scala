package org.icar.actor_model.core

import java.awt.Dimension

import akka.actor.{Actor}
import javax.swing.{JFrame, JTabbedPane, JTextArea}

import scala.concurrent.ExecutionContextExecutor

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

	val my_log_area : MUSALogger

	def mylog(string:String) : Unit = {
		my_log_area.mylog(string)
	}
}

abstract class MUSALogger {
	def mylog(string:String) : Unit
}
abstract class MUSALoggerFactory {
	def register_actor(name:String) : MUSALogger
}

class MultiTabLogger(title:String) extends MUSALoggerFactory {
	val frame = new JFrame(title)
	val tabbed = new JTabbedPane()
	frame.getContentPane.add(tabbed)
	frame.setPreferredSize(new Dimension(600, 500))
	frame.pack
	frame.setVisible(true)

	override def register_actor(name: String): MUSALogger = {
		val tab = new SingleTab
		tabbed.add(name,tab.my_text_area)
		tab
	}
}

class SingleTab extends MUSALogger {
	val my_text_area = new JTextArea()

	override def mylog(string: String): Unit = {
		my_text_area.append(string+"\n")
	}
}

/*
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
*/




