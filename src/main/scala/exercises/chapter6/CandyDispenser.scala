package exercises.chapter6

import main.scala.exercises.chapter6.State._

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int) {
  def checkRules(input: Input) = input match {
    case _ if candies == 0 => this
    case Coin if locked => Machine(false, candies, coins - 1)
    case Turn if !locked => Machine(true, candies - 1, coins)
    case _ => this
  }
}

//object CandyDispenser {
//  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
//    _ <- sequence(inputs.map(input => modify[Machine](_.checkRules(input))))
//    machine <- get
//  } yield (machine.candies, machine.coins)
//}
