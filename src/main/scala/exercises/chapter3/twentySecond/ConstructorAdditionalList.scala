package exercises.chapter3.twentySecond

import exercises.chapter3.{Cons, MyList, Nil}

object ConstructorAdditionalList extends App {

  def addElementsList(xs: MyList[Int], ys: MyList[Int]): MyList[Int] = (xs, ys) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, addElementsList(xs, ys))
  }

  println(addElementsList(MyList(1, 2, 3), MyList(3, 1, 2)))

}
