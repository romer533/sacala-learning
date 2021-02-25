package akka.actors

import akka.actor.{Actor, ActorSystem, Props, Status}
import akka.pattern.ask
import akka.util.Timeout

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success}

sealed trait Command

case object Fill
case object Take

class CoffeeMachine extends Actor {

  import context._
  def initialized(current: Int): Receive = {
    case Fill if current < 3 =>
      sender() ! s"Cup filled || $current"
      become(initialized(current + 1))
    case Fill =>
      sender() ! Status.Failure(new IllegalStateException) // s"Have no places for cup || $current
    case Take =>
      sender() ! println("Something")
  }

  override def preStart(): Unit = become(initialized(0))

  def receive: Receive = Actor.emptyBehavior

}

object Main extends App {
  val system = ActorSystem("coffee")

  val coffeeMachine = system.actorOf(Props[CoffeeMachine])

  implicit val timeout: Timeout = Timeout(10, TimeUnit.DAYS)

  import system.dispatcher

  coffeeMachine ! Fill
  coffeeMachine ! Fill
  coffeeMachine ! Fill
  (coffeeMachine ? Fill).onComplete {
    case Failure(exception) => exception.printStackTrace()
    case Success(value) => println(value)
  }
  coffeeMachine ! Fill
  coffeeMachine ! Fill
  coffeeMachine ! Take
}