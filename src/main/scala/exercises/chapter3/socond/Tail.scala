package exercises.chapter3.socond

import exercises.chapter3.{Cons, MyList}

object Tail extends App {

  def tail[A](as: MyList[A]) = as match {
    case Cons(_, t) => t
//    case Nil => ???
  }

  println(tail(MyList(1, 2, 3)))

}