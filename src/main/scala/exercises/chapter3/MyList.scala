package exercises.chapter3

import scala.annotation.tailrec

sealed trait MyList[+A]
case object Nil extends MyList[Nothing]
case class Cons[+A](head: A, tail: MyList[A]) extends MyList[A]

object MyList extends App {

  def sum(ints: MyList[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: MyList[Double]): Double = ds match {
    case Nil => 1.0
    //    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): MyList[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def empty[A]: MyList[A] = Nil

  def foldRight[A, B](as: MyList[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def sum2(ns: MyList[Int]): Int = foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: MyList[Double]): Double = foldRight(ns, 1.0)(_ * _)

  def foldLeft[A, B](as: MyList[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(h, t) => f(foldLeft(t, z)(f), h)
  }

  def sum3(ns: MyList[Int]): Int = foldLeft(ns, 0)(_ + _)

  def product3(ns: MyList[Double]): Double = foldLeft(ns, 1.0)(_ * _)

  def lengthLeft(ns: MyList[Any]): Int = foldLeft(ns, 0)((n, _) => n + 1)

  def foldLeft2[A, B](as: MyList[A], z: B)(f: (B, A) => B): B =
    foldRight(as, (b: B) =>  b)((a, g) => b => g(f(b, a)))(z)

  def foldRight2[A, B](as: MyList[A], z: B)(f: (A, B) => B): B =
    foldLeft(as, (b: B) => b)((g, b) => a => g(f(b, a)))(z)

  def append[A](xs: MyList[A], ys: MyList[A]): MyList[A] =
    foldRight(xs, ys)((a, acc) => Cons(a, acc))

  def flatMap[A, B](as: MyList[A])(f: A => MyList[B]): MyList[B] = as match {
    case Nil => Nil
    case Cons(h, t) => MyList.append(f(h), flatMap(t)(f))
  }

  def filter2[A, B](as: MyList[A])(f: A => Boolean): MyList[A] =
    flatMap(as)(a => if (f(a)) MyList(a) else Nil)

  def length[A](as: MyList[A]): Int = MyList.foldRight(as, 0)((_, n) => n + 1)

  // Не работает, если на вход дать MyList(1, 1, 2, 3) и MyList(1, 2)
  @tailrec
  def hasSubsequence[A](sup: MyList[A], sub: MyList[A]): Boolean = (sup, sub) match {
    case (Cons(h1, _), Cons(h2, Nil)) => h1 == h2
    case (Cons(h1, t1), sub @ Cons(h2, t2)) => if (h1 == h2) hasSubsequence(t1, t2)  else hasSubsequence(t1, sub)
  }

}

