package main.scala.exercises.chapter5

sealed trait MyStream[+A]

case object MyEmpty extends MyStream[Nothing]

case class MyCons[+A](h: () => A, t: () => MyStream[A]) extends MyStream[A]


object MyStream extends App {

  def myCons[A](hd: => A, t1: MyStream[A]): MyStream[A] = {
    lazy val head = hd
    lazy val tail = t1
    MyCons(() => head, () => tail)
  }

  def myEmpty[A]: MyStream[A] = MyEmpty

  def myApply[A](as: A*): MyStream[A] =
    if (as.isEmpty) myEmpty else myCons(as.head, myApply(as.tail: _*))

}