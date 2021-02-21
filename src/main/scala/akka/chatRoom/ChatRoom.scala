package akka.chatRoom

object ChatRoom {

  private final case class PublishSessionMessage(screenName: String, message: String) extends RoomCommand

}
