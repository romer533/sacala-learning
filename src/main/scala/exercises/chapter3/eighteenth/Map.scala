package exercises.chapter3.eighteenth

import exercises.chapter3.{Cons, MyList, Nil}

object Map extends App {

  def map[A, B](as: MyList[A])(f: A => B): MyList[B] = as match {
    case Nil => Nil
    case Cons(h, t) => Cons(f(h), map(t)(f))
  }

  println(map(MyList(1, 2, 3))(???))

}
