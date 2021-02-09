package exercises.chapter3.twelfth

import exercises.chapter3.{Cons, MyList, Nil}

import scala.annotation.tailrec

object Reverse extends App {

  def reverse[A](as: MyList[A]): MyList[A] =  {
    @tailrec
    def loop(as: MyList[A], acc: MyList[A]): MyList[A] = as match {
      case Nil => acc
      case Cons(h, t) => loop(t, Cons(h, acc))
    }
    loop(as, Nil)
  }

  println(reverse(MyList(1, 2, 3)))

}
