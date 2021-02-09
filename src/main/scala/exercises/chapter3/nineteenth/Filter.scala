package exercises.chapter3.nineteenth

import exercises.chapter3.{Cons, MyList, Nil}

object Filter extends App {

  // ????????????????????
  def filter[A](as: MyList[A])(f: A => Boolean): MyList[A] = ???
//    MyList.foldRight(as, MyList())((a, acc) => if (f(a)) Cons(a, acc) else acc)

  println(filter(MyList(1, 2, 3))(f => f > 1))

}
