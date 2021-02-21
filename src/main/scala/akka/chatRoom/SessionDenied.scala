package akka.chatRoom

final case class SessionDenied(reason: String) extends SessionEvent
