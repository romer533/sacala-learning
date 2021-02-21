package akka.chatRoom

private final case class NotifyClient(message: MessagePosted) extends SessionCommand
