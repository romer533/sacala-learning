package exercises.chapter4

import scala.annotation.tailrec

sealed trait MyEither[+E, +A] {

  def map[B](f: A => B): MyEither[E, B] = this match {
    case MyRight(a) => MyRight(f(a))
    case MyLeft(a) => MyLeft(a)
  }

  def flatMap[EE >: E, B](f: A => MyEither[EE, B]): MyEither[EE, B] = this match {
    case MyRight(a) => f(a)
    case MyLeft(a) => MyLeft(a)
  }

  def orElse[EE >: E, B >: A](b: => MyEither[EE, B]): MyEither[EE, B] = this match {
    case MyLeft(_) => b
    case _ => this
  }

  def map2[EE >: E, B, C](b: MyEither[EE, B])(f: (A, B) => C): MyEither[EE, C] = (this, b) match {
    case (MyRight(a), MyRight(b)) => MyRight(f(a, b))
    case (_, MyLeft(b)) => MyLeft(b)
    case (MyLeft(a), _) => MyLeft(a)
  }

}

case class MyLeft[+E](value: E) extends MyEither[E, Nothing]

case class MyRight[+A](value: A) extends MyEither[Nothing, A]

object MyEitherMain extends App {

  def mean(xs: IndexedSeq[Double]): MyEither[String, Double] =
    if (xs.isEmpty) MyLeft("mean of empty list!")
    else MyRight(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): MyEither[Exception, Int] =
    try MyRight(x / y)
    catch {
      case e: Exception => MyLeft(e)
    }

  def myTry[A](a: => A): MyEither[Exception, A] =
    try MyRight(a)
    catch {
      case e: Exception => MyLeft(e)
    }


  def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = ???

  def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): MyEither[Exception, Double] =
    for {
      a <- myTry {
        age.toInt
      }
      tickets <- myTry {
        numberOfSpeedingTickets.toInt
      }
    } yield insuranceRateQuote(a, tickets)

  def sequence[E, A](as: List[MyEither[E, A]]): MyEither[E, List[A]] = {
    @tailrec
    def iter(as: List[MyEither[E, A]], acc: List[A]): MyEither[E, List[A]] = as match {
      case (left@MyLeft(_)) :: _ => left
      case MyRight(a) :: t => iter(t, a :: acc)
      case Nil => MyRight(acc.reverse)
    }

    iter(as, Nil)
  }

  def traverse[E, A, B](as: List[A])(f: A => MyEither[E, B]): MyEither[E, List[B]] = {
    @tailrec
    def iter(as: List[A], acc: List[B]): MyEither[E, List[B]] = as match {
      case h :: t => f(h) match {
        case left@MyLeft(_) => left
        case MyRight(b) => iter(t, b :: acc)
      }
      case Nil => MyRight(acc.reverse)
    }

    iter(as, Nil)
  }


  case class Person(name: Name, age: Age)

  sealed class Name(val value: String)

  sealed class Age(val value: Int)

  def mkName(name: String): MyEither[String, Name] =
    if (name == "" || name == null) MyLeft("Name is empty.")
    else MyRight(new Name(name))

  def mkAge(age: Int): MyEither[String, Age] =
    if (age < 0) MyLeft("Age is out of range.")
    else MyRight(new Age(age))

  def mkPerson(name: String, age: Int): MyEither[String, Person] =
    mkName(name).map2(mkAge(age))(Person(_, _))

  def map2b[E, B, C, D](e1: MyEither[E, B], e2: MyEither[E, C])(f: (B, C) => D): MyEither[List[E], D] = (e1, e2) match {
    case (MyRight(b), MyRight(c)) => MyRight(f(b, c))
    case _ => MyLeft(List(e1, e2) collect { case MyLeft(e) => e })
  }

}