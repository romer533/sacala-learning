package exercises.chapter3.fifth

import exercises.chapter3.{Cons, MyList}

import scala.annotation.tailrec

object DropWhile extends App {

  @tailrec
  def dropWhile[A](l: MyList[A], f: A => Boolean): MyList[A] = l match {
    case Cons(h, t) =>
      if (f(h)) dropWhile(t, f)
      else l
  }

  println(dropWhile(MyList(1, 2, 3, 4), (f: Int) => f < 3))

}
