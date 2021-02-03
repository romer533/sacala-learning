package exercises.chapter2.fourth

object Uncurry extends App {

  def uncurry[A, B, C] (f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)

}
