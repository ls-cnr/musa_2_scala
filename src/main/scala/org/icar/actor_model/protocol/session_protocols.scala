package org.icar.actor_model.protocol

import org.icar.actor_model.core.{Protocol, ProtocolPart}

package object CreateSessionProtocol extends Protocol {
	def init() : ProtocolPart = NewSessionRequest(get_id)

	trait NewSessionProtocolPart extends ProtocolPart

	case class NewSessionRequest private(id:Long) extends NewSessionProtocolPart {
		def success(session_id:String) : ProtocolPart = NewSessionResponse(this.id,session_id)
	}
	case class NewSessionResponse private(id:Long,session_id:String) extends NewSessionProtocolPart
}

package object RemoveSessionProtocol extends Protocol {
	def init(session_id:String) : ProtocolPart = SessionRemoveRequest(get_id,session_id)

	trait RemoveSessionProtocolPart extends ProtocolPart

	case class SessionRemoveRequest private(id:Long,session_id:String) extends RemoveSessionProtocolPart {
		def success() : ProtocolPart = RemovedSuccessfullyResponse(this.id)
		def failure() : ProtocolPart = RemovedNotSuccessfullyResponse(this.id)
	}
	case class RemovedSuccessfullyResponse private(id:Long) extends RemoveSessionProtocolPart
	case class RemovedNotSuccessfullyResponse private(id:Long) extends RemoveSessionProtocolPart
}
