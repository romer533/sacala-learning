package exercises.chapter3.twentyFourth

import exercises.chapter3.{Cons, MyList, Nil}

import scala.annotation.tailrec

object HasSubsequence extends App {


  // ???????
  @tailrec
  def hasSubsequence[A](sup: MyList[A], sub: MyList[A]): Boolean = (sup, sub) match {
    case (Nil, _) => false
    case (_, Nil) => false
    case (Cons(h1, t1), Cons(h2, t2)) => if (h1 == h2) true else hasSubsequence(t1, t2)
  }

  println(hasSubsequence(MyList(1, 2, 3, 4, 5), MyList(6, 7, 1, 2, 5)))
  println(hasSubsequence(MyList(1, 2, 3, 4, 5), MyList(4, 5)))

}
