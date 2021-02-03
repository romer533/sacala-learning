package exercises.chapter2.third

object Curry extends App {

  def curry[A, B, C] (f: (A, B) => C): A => (B => C) =
    (a: A) => (b: B) => f(a, b)

}
