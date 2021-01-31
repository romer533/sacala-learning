package helloWorld

import scala.io.StdIn

object HelloNameApp extends App {

  val name = StdIn.readLine("He! Enter your name: ")

  println(s"Hello $name!")

}
