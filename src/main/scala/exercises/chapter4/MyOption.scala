package exercises.chapter4

import exercises.chapter3.MyList

import scala.util.Try

sealed trait MyOption[+A] {

  def map[B](f: A => B): MyOption[B] = this match {
    case MyNone => MyNone
    case MySome(a) => MySome(f(a))
  }

  def flatMap[B](f: A => MyOption[B]): MyOption[B] = this match {
    case MyNone => MyNone
    case MySome(a) => f(a)
  }

  def getOrElse[B >: A](default: => B): B = this match {
    case MySome(a) => a
    case _ => default
  }

  def orElse[B >: A](ob: => MyOption[B]): MyOption[B] = this match {
    case MyNone => ob
    case _ => this
  }

  def filter(f: A => Boolean): MyOption[A] = this match {
    case MySome(a) if f(a) => this
    case _ => MyNone
  }

}

case class MySome[+A](get: A) extends MyOption[A]
case object MyNone extends MyOption[Nothing]

object MyOptionMain extends App {

  case class MyEmployee(name: String, department: String)

  //  def lookupByName(name: String): MyOption[MyEmployee] = ???

  //  val joeDepartment: MyOption[String] =
  //    lookupByName("Joe").map(_.department)

  def mean(xs: Seq[Double]): MyOption[Double] =
    if (xs.isEmpty) MyNone
    else MySome(xs.sum / xs.length)

  def variance(xs: Seq[Double]): MyOption[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  def lift[A, B](f: A => B): MyOption[A] => MyOption[B] = _ map f

  val abs0: MyOption[Double] => MyOption[Double] = lift(math.abs)

    def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = ???

    def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): MyOption[Double] = {
      val optAge: MyOption[Int] = myTry(age.toInt)
      val optTickets: MyOption[Int] = myTry(numberOfSpeedingTickets.toInt)
      map2(optAge, optTickets)(insuranceRateQuote)
    }

  def myTry[A](a: => A): MyOption[A] =
    try MySome(a)
    catch { case e: Exception => MyNone }

  def map2[A, B, C](a: MyOption[A], b: MyOption[B])(f: (A, B) => C): MyOption[C] = (a, b) match {
    case (MySome(a), MySome(b)) => MySome(f(a, b))
    case _ => MyNone
  }

  def sequence[A](a: List[MyOption[A]]): MyOption[List[A]] =
    a.foldRight(MySome(List.empty[A]): MyOption[List[A]]) {
      case (a, myOptionAcc) => myOptionAcc.flatMap(acc => a.map(_ :: acc))
    }

  def parseInts(a: List[String]): MyOption[List[Int]] =
    sequence(a map (i => myTry(i.toInt)))

  def traverse[A, B](a: List[A])(f: A => MyOption[B]): MyOption[List[B]] = a match {
    case Nil => MySome(Nil)
    case h :: t => f(h).flatMap(fh => traverse(t)(f).map(fh :: _))
  }

  def sequence2[A](a: List[MyOption[A]]): MyOption[List[A]] = traverse(a)(identity)

}
