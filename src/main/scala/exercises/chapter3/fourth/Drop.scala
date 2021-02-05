package exercises.chapter3.fourth

import scala.annotation.tailrec

object Drop extends App {

  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case _ :: xs => {
      if (n <= 0) xs
      else drop(xs, n - 1)
    }
  }

}
