package exercises.chapter3.sixteenth

import exercises.chapter3.{Cons, MyList, Nil}

object AddFunction extends App {

  def addFunction(as: MyList[Int]): MyList[Int] = as match {
    case Nil => Nil
    case Cons(h, t) => Cons(h + 1, addFunction(t))
  }

  println(addFunction(MyList(1, 2, 3)))

}
