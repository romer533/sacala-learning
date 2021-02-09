package exercises.chapter3.twentieth

import exercises.chapter3.{Cons, MyList, Nil}

import scala.annotation.tailrec

object FlatMap extends App {

  def flatMap[A, B](as: MyList[A])(f: A => MyList[B]): MyList[B] = as match {
    case Nil => Nil
//    case Cons(h, t) => f(h) + flatMap(t)(f)
  }

  println(flatMap(MyList(1, 2, 3))(i => MyList(i, i)))

}
