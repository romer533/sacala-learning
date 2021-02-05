package exercises.chapter3.sixth

import exercises.chapter3.{Cons, MyList, Nil}

object Init extends App {

  def init[A](l: MyList[A]): MyList[A] = l match {
    case Cons(_, Nil) =>
      Nil
    case Cons(h, t) =>
      Cons(h, init(t))
  }

  println(init(MyList(1, 2, 3, 4)))
}
