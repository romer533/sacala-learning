package exercises.chapter3.seventeenth

import exercises.chapter3.{Cons, MyList, Nil}

object DoubleToString extends App {

  def doubleToString(as: MyList[Double]): MyList[String] = as match {
    case Nil => Nil
    case Cons(h, t) => Cons(h.toString, doubleToString(t))
  }

  println(doubleToString(MyList(1.2, 2.3, 3.74)))

}
