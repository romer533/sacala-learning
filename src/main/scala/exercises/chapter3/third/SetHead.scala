package exercises.chapter3.third

object SetHead extends App {

  def setHead[A](as: List[A]) = as match {
    case x :: _ => x
    case Nil => ???
  }

}
