package akka.chatRoom

final case class MessagePosted(screenName: String, message: String) extends SessionEvent
