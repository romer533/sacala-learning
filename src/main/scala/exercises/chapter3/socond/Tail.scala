package exercises.chapter3.socond

object Tail extends App {

  def tail[A](as: List[A]) = as match {
    case _ :: xs => xs
    case Nil => ???
  }

}