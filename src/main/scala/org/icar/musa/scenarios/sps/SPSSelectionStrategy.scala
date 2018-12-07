package org.icar.musa.scenarios.sps

import java.awt.event.{ActionEvent, ActionListener}

import javax.swing._
import org.icar.musa.pmr.Solution
import org.icar.musa.specification.SelectionStrategy

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration._

class SPSSelectionStrategy extends SelectionStrategy with ActionListener {
  var count = 0
  var solutions : ArrayBuffer[Solution] = ArrayBuffer()
  var opt_selected : Option[Solution]  = None

  var model = new DefaultListModel[String]
  val gui = new SolutionsGUI(model,this)


  def init : Unit = {
    gui.setVisible(true)
  }

  def update(sol : Solution) : Unit = {
    solutions += sol
    count += 1
    model.addElement("solution "+count+" => "+sol.inlineString)
  }

  def check_selection : Option[Solution] = opt_selected

  def terminate : Unit = { gui.setVisible(false) /*frame.setVisible(false)*/ }

  def check_delay : FiniteDuration = 10 seconds

  override def actionPerformed(e: ActionEvent) : Unit = {
    val index = gui.getSelectedForExecution
    if (index != -1) {
      val selected: Solution = solutions(index)
      opt_selected=Some(selected)
      //println("SELECTED " + selected.inlineString)
      gui.setVisible(false)
    }

  }
}