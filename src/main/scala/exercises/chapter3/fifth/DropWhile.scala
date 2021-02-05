package exercises.chapter3.fifth

import exercises.chapter3.{Cons, MyList}

object DropWhile extends App {

  def dropWhile[A](l: MyList[A], f: A => Boolean): MyList[A] = l match {
    case Cons(h, t) => {
      if (f(h)) t
      else Cons(h, t)
    }
  }

}
