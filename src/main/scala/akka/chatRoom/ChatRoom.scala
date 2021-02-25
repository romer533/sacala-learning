package akka.chatRoom

import akka.actor.typed.scaladsl.{AbstractBehavior, ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, Behavior}

import java.net.URLEncoder
import java.nio.charset.StandardCharsets

object ChatRoom {

  private final case class PublishSessionMessage(screenName: String, message: String) extends RoomCommand

  def apply(): Behavior[RoomCommand] =
    Behaviors.setup(context => new ChatRoomBehavior(context))

  private def chatRoom(sessions: List[ActorRef[SessionCommand]]): Behavior[RoomCommand] =
    Behaviors.receive { (context, message) =>
      message match {
        case GetSession(screenName, client) =>
          val ses = context.spawn(
            session(context.self, screenName, client),
            name = URLEncoder.encode(screenName, StandardCharsets.UTF_8.name))
          client ! SessionGranted(ses)
          chatRoom(ses :: sessions)
        case PublishSessionMessage(screenName, message) =>
          val notification = NotifyClient(MessagePosted(screenName, message))
          sessions.foreach(_ ! notification)
          Behaviors.same
      }
    }

  def aga[A](as: List[Option[A]]): Option[List[A]] = {
    val empty: Option[List[A]] = Some(Nil)
    as.foldLeft(empty) { (acc, el) =>
      el match {
        case Some(value) =>
          acc match {
            case Some(list) =>
              Some(value +: list)
            case None => None
          }
        case None => None
      }

    }
  }

  def aga2[A](as: List[Option[A]]): Option[List[A]] = {
    val empty: Option[List[A]] = Some(Nil)
    as.foldLeft(empty)((acc, el) => el.flatMap(value => acc.map(list => list :+ value)))
  }

  private def session(room: ActorRef[PublishSessionMessage],
                      screenName: String,
                      client: ActorRef[SessionEvent]): Behavior[SessionCommand] =
    Behaviors.receiveMessage {
      case PostMessage(message) =>
        room ! PublishSessionMessage(screenName, message)
        Behaviors.same
      case NotifyClient(message) =>
        client ! message
        Behaviors.same
    }

  class ChatRoomBehavior(context: ActorContext[RoomCommand]) extends AbstractBehavior[RoomCommand](context) {
    private var session: List[ActorRef[SessionCommand]] = List.empty

    override def onMessage(msg: RoomCommand): Behavior[RoomCommand] = ???
  }

}