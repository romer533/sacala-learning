package exercises.twoPointOne

object Fibonacci extends App {

  println(fib(5))

  // Versional 1.0
  def fib(n: Int): Int =
    if (n <= 1) n
    else fib(n - 1) + fib(n - 2)

  // Versional 2.0
  val fib3: Int => Int = (n: Int) =>
  if (n <= 1) n
  else fib3(n - 1) + fib3(n - 2)

  // Versional 3.0
  val fib2: Int => Int = n =>
    if (n <= 1) n
    else fib2(n - 1) + fib2(n - 2)

  println(fib2(5))
  println(fib3(5))

}