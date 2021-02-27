package akka.actors.persistence

import akka.actor._
import akka.persistence._

case class Cmd(data: String)
case class Evt(data: String)

case class ExampleState(events: List[String] = Nil) {
  def updated(evt: Evt): ExampleState = copy(evt.data :: events)
  def size: Int = events.length
  override def toString: String = events.reverse.toString
}

class ExamplePersistentActor extends PersistentActor {
  override def persistenceId = "sample-id-1"

  var state: ExampleState = ExampleState()

  def updateState(event: Evt): Unit =
    state = state.updated(event)

  def numEvents: Int =
    state.size

  val receiveRecover: Receive = {
    case evt: Evt => updateState(evt)
    case SnapshotOffer(_, snapshot: ExampleState) => state = snapshot
  }

  val snapShotInterval = 10
  val receiveCommand: Receive = {
    case Cmd(data) =>
      println(s"Something message - start $data")
      persist(Evt(s"$data-$numEvents")) { event =>
        updateState(event)
        context.system.eventStream.publish(event)
        if (lastSequenceNr % snapShotInterval == 0 && lastSequenceNr != 0)
          saveSnapshot(state)
      }
      println(s"Something message - end $data")
    case "print" => println(state)
  }
}

object ExamplePersistentActorApp extends App {

  val system = ActorSystem("example")
  val persistentActor = system.actorOf(Props[ExamplePersistentActor](), "persistentActor-4-scala")

  persistentActor ! "print"
  persistentActor ! Cmd("foo")
  persistentActor ! Cmd("baz")
  persistentActor ! Cmd("bar")
  persistentActor ! Cmd("buzz")
  persistentActor ! "print"

  Thread.sleep(5000)
  system.terminate()

}
