package exercises.chapter2.second

import scala.annotation.tailrec

object ArraySort extends App {



  def isSorted[A] (as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @tailrec
    def loop(n: Int): Boolean = {
      if (as.length == 0 || as.length == 1) false
      else if (n == 0) true
      else if (!ordered(as(n - 1), as(n))) false
      else loop(n - 1)
    }

    loop(as.length - 1)
  }

}
