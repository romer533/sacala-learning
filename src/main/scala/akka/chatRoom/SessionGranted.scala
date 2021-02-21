package akka.chatRoom

import akka.actor.typed.ActorRef

final case class SessionGranted(handle: ActorRef[PostMessage]) extends SessionEvent