package org.icar.musa.specification

import org.icar.musa.actor_model.RequestNewSession

import scala.concurrent.duration.FiniteDuration

abstract class ProxyStrategy extends Runnable {
  var my_callback : RequestNewSession => Unit = null

  def set_callback (callback : RequestNewSession => Unit) = {my_callback = callback}

}


abstract class TimedProxyStrategy(every: FiniteDuration) extends ProxyStrategy {

  override def run(): Unit = {

    while (my_callback != null) {
      val request = generate_new_request
      my_callback(request)
      Thread.sleep(every.toMillis)
    }

  }

  def generate_new_request : RequestNewSession
}

class NoProxyStrategy extends ProxyStrategy {
  override def run(): Unit = {}
}