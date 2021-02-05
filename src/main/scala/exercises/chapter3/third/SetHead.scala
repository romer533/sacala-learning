package exercises.chapter3.third

import exercises.chapter3.{Cons, MyList}

object SetHead extends App {

  def setHead[A](as: MyList[A]) = as match {
    case Cons(h, _) => h
//    case Nil => ???
  }

  println(setHead(MyList(1, 2, 3, 4)))

}
