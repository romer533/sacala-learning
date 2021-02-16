package main.scala.exercises.chapter5

import main.scala.exercises.chapter5.Stream.{cons, empty}

sealed trait Stream[+A] {

  def toList: List[A] = this match {
    case Cons(h, t) => h() :: t().toList
    case Empty => Nil
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

//  def takeWhile(p: A => Boolean): Stream[A] = this match {
//    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
//    case _ => empty
//  }

//  def exists(p: A => Boolean): Boolean = this match {
//    case Cons(h, t) => p(h()) || t().exists(p)
//    case _ => false
//  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def exists(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean = foldRight(true)((a, b) => p(a) && b)

  def takeWhile(p: A => Boolean): Stream[A] =
    foldRight[Stream[A]](empty)((a, b) => if (p(a)) cons(a, b) else empty)

  def headOption: Option[A] =
    foldRight[Option[A]](None)((h, _) => Some(h))

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((h, t) => cons(f(h), t))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) =>
    if (p(h)) cons(h, t)
    else t
    )

  def append[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s)((h, t) => cons(h, t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h, t) => f(h).append(t))

  def find(p: A => Boolean): Option[A] =
    filter(p).headOption

}


case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]


object Stream extends App {

  def cons[A](hd: => A, t1: Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = t1
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  def constant[A](a: A): Stream[A] = Stream.cons(a, constant(a))

  def from(n: Int): Stream[Int] = Stream.cons(n, from(n + 1))

  def fibs(a: Int, b: Int): Stream[Int] = Stream.cons(a, fibs(b, b + a))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((a, s)) => Stream.cons(a, unfold(s)(f))
    case None => Stream.empty[A]
  }

  def fibs2 = Stream.cons(0, Stream.cons(1, unfold((0, 1)) {
    case (a, b) =>
      val c = a + b
      Some(c, (b, c))
  }))

  def from2(n: Int) = unfold(n) { n => Some((n, n + 1)) }

  def constant2(n: Int) = unfold(n) { n => Some((n, n)) }

  def ones2 = unfold(1) { one => Some((one, one)) }

}