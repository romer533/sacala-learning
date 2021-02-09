package exercises.chapter3.twentyThird

import exercises.chapter3.{Cons, MyList, Nil}

object ZipWith extends App {

  def zipWith[A, B, C](as: MyList[A], bs: MyList[B])(f: (A, B) => C): MyList[C] = (as, bs) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(a, as), Cons(b, bs)) => Cons(f(a, b), zipWith(as, bs)(f))
  }

}
