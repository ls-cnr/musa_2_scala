package org.icar.actor_model.core

import java.awt.Dimension

import javax.swing.{JFrame, JTabbedPane, JTextArea}

class MultiTabLogger(title:String) extends MUSALoggerFactory {
	val frame = new JFrame(title)
	val tabbed = new JTabbedPane()

	init

	def init : Unit = {
		frame.getContentPane.add(tabbed)
		frame.setPreferredSize(new Dimension(600, 500))
		frame.pack()
		frame.setVisible(true)
	}

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
