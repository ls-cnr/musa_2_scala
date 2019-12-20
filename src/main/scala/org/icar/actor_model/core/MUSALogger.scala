package org.icar.actor_model.core

abstract class MUSALogger {
	def mylog(string:String) : Unit
}
abstract class MUSALoggerFactory {
	def register_actor(name:String) : MUSALogger
}


class ConsoleLogger extends MUSALogger {
	def mylog(string:String) : Unit = println(string)
}
class ConsoleLoggerFactory extends MUSALoggerFactory {
	def register_actor(name:String) : MUSALogger = new ConsoleLogger()
}


