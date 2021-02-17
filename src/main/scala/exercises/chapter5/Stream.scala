package main.scala.exercises.chapter5

import main.scala.exercises.chapter5.Stream.{cons, empty, unfold}

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

  def map2[B](f: A => B): Stream[B] = unfold(this) {
    case Cons(h, t) => Some((f(h()), t()))
    case _ => None
  }

  def take2(n: Int): Stream[A] = unfold((this, n)) {
    case (Cons(h, _), 1) => Some((h(), (empty, 0)))
    case (Cons(h, t), i) if i > 1 => Some((h(), (t(), i - 1)))
    case _ => None
  }

  def takeWhile2(p: A => Boolean): Stream[A] = unfold(this) {
    case Cons(h, t) if p(h()) => Some((h(), t()))
    case _ => None
  }

  def zipWith[B, C](s2: Stream[B])(f: (A, B) => C): Stream[C] = unfold((this, s2)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
    case _ => None
  }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = unfold((this, s2)) {
    case (Cons(h1, t1), Cons(h2, t2)) => Some(((Some(h1()), Some(h2())), (t1(), t2())))
    case (Cons(h1, t1), Empty) => Some(((Some(h1()), None), (t1(), empty)))
    case (Empty, Cons(h2, t2)) => Some(((None, Some(h2())), (empty, t2())))
    case _ => None
  }

  def startsWith[A](s: Stream[A]): Boolean =
    zipAll(s) takeWhile {
      case (_, b) => b != None
    } forAll {
      case (a, b) => a == b
    }

  def tails: Stream[Stream[A]] = unfold(this) {
    case s @ Cons(_, t) => Some((s, t()))
    case Empty => None
  } append Stream(empty)

  def hasSubsequence[A](s: Stream[A]): Boolean = tails exists(_ startsWith s)

  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
    foldRight((z, Stream(z)))((a, b) => {
      lazy val lb = b
      val x = f(a, lb._1)
      (x, cons(x, lb._2))
    })._2

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