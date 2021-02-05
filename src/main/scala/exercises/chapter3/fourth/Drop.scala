package exercises.chapter3.fourth

import exercises.chapter3.{Cons, MyList}

import scala.annotation.tailrec

object Drop extends App {

  @tailrec
  def drop[A](l: MyList[A], n: Int): MyList[A] = l match {
    case Cons(_, t) => {
      if (n <= 0) t
      else drop(t, n - 1)
    }
  }

}
