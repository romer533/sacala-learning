package exercises.twoPointOne

object Fibonacci extends App {

  println(fib(5))

  def fib(n: Int): Int = {
    if (n <= 1) n
    else fib(n - 1) + fib(n - 2)
  }

}
