package akka.actors

import akka.actor.{Actor, ActorSystem, Props, Status}
import akka.pattern.ask
import akka.util.Timeout

import java.util.concurrent.TimeUnit
import scala.concurrent.Future
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
      become(initialized(current - 1))
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





//  coffeeMachine ! Fill


//  (coffeeMachine ? Fill).mapTo[String]
//    .flatMap { s =>
//      (coffeeMachine ? Fill).mapTo[String].flatMap { s2 =>
//        (coffeeMachine ? Fill).mapTo[String].map { s3 =>
//            List(s, s2, s3)
//        }
//      }
//    }

  (for {
    s <- (coffeeMachine ? Fill).mapTo[Int]
    s2 <- (coffeeMachine ? Fill).mapTo[Int]
    s3 <- (coffeeMachine ? Fill).mapTo[Int]
  } yield Seq(s, s2, s3))

  def mySeq(data: Seq[Future[Int]]): Future[Seq[Int]] = {
    val empty: Future[Seq[Int]] = Future.successful(Seq.empty)
    data.foldLeft(empty)((acc, el) => el.flatMap {
      value =>
        acc.map {
          list =>
            value +: list
        }
    })
  }

  def batchedFutures(l: List[Int], batchSize: Int, f: Int => Future[Int]): Future[Seq[Int]] = {
    val empty: Future[List[Int]] = Future.successful(List.empty)
    val groupedList: List[List[Int]] = l.grouped(batchSize).toList
    groupedList.foldLeft(empty)((acc, el) => mySeq(el.map(f)).flatMap(s => acc.map(s ++: _)))
  }

  //  (coffeeMachine ? Fill).onComplete {
//    case Failure(exception) => exception.printStackTrace()
//    case Success(value) => println(value)
//  }
//  coffeeMachine ! Fill
//  coffeeMachine ! Fill
//  coffeeMachine ! Take
}