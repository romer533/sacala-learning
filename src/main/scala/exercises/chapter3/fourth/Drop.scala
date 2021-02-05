package exercises.chapter3.fourth

object Drop extends App {

  def drop[A](l: List[A], n: Int): List[A] = l match {
    case _ :: xs => {
      if (n <= 0) xs
      else drop(xs, n - 1)
    }
  }

}
