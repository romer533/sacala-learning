package akka.actors

import akka.actor.{Actor, ActorSystem, Props, Status}
import akka.actors.persistence.{Evt, ExampleState}
import akka.pattern.ask
import akka.persistence.PersistentActor
import akka.util.Timeout

import java.util.concurrent.TimeUnit
import scala.util.{Failure, Success}

case class BuyPlace(isBuy: Boolean)
case class BuyPillow(isBuy: Boolean)
case class BuyPlaid(isBuy: Boolean)
case class BuyMattress(isBuy: Boolean)

case class State(events: List[String] = Nil) {
  def updated(evt: Evt): State = copy(evt.data :: events)
  def size: Int = events.length
  override def toString: String = events.reverse.toString
}

class Cinema extends PersistentActor {

  // TODO: Создать реализацию работы кинотеатра с разделением по часам,
  //  с разными плюхами для мест (Подушечка, плед, матрас),
  //  матрас нельзя купить без подушки и пледа. Использовать Option, Either, Try

  import context._
  def initialized: Receive = {
    case BuyPlace => ??? // Если мы не покупаем место, нам не доступна покупка полушки и пледа
    case BuyPillow => ???
    case BuyPlaid => ???
    case BuyMattress => ??? // Если мы не покупаем подушку и плед, нам не доступна покупка матраса
  }

  override def preStart(): Unit = become(initialized)

//  def receive: Receive = Actor.emptyBehavior
  override def receiveRecover: Receive = ???

  override def receiveCommand: Receive = ???

  override def persistenceId: String = ???
}

object Main extends App {
  val system = ActorSystem("Zone 1")

  val zone1 = system.actorOf(Props[CoffeeMachine])

  implicit val timeout: Timeout = Timeout(10, TimeUnit.DAYS)

  import system.dispatcher
  (for {
    s1 <- (zone1 ? BuyPlace).mapTo[String]
    s2 <- (zone1 ? BuyPillow).mapTo[String]
    s3 <- (zone1 ? BuyPlaid).mapTo[String]
    s4 <- (zone1 ? BuyMattress).mapTo[String]
  } yield Seq(s1, s2, s3, s4)).onComplete {
    case Failure(exception) => ??? // Выполнится, если была попытка купить что-то раньше чего-то
    case Success(value) => ??? // Выполнится, если все покупки прошли без исключений
  }

}