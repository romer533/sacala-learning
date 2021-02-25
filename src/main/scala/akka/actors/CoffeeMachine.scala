package akka.actors

import akka.actor.{Actor, ActorRef, ActorSystem, Props}

import scala.concurrent.duration.DurationInt

case object Fill
case object Take


class Cup extends Actor {
  def receive = {
    case Take =>
      println("Cup tacked")
      sender() ! Fill
  }
}

class CoffeeMachine(cup: ActorRef) extends Actor {
  var current = 0

  def receive = {
    case Fill =>
      if (current < 3) {
        current += 1
        println(s"Cup filled $current")
      } else {
        current -= 1
        cup ! Take
        println(s"Have not places for cups $current")
      }
  }
}

object Main extends App {
  val system = ActorSystem("coffee")

  val cup = system.actorOf(Props[Cup](), "cup")

  val coffeeMachine = system.actorOf(Props(classOf[CoffeeMachine], cup), "coffeeMachine")

  for (_ <- 0 to 10) coffeeMachine ! Fill
}