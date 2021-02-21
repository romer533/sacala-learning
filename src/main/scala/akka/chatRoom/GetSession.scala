package akka.chatRoom

import akka.actor.typed.ActorRef

final case class GetSession(screenName: String, replyTo: ActorRef[SessionEvent]) extends RoomCommand
