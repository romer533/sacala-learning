package exercises.chapter3.twentySixth

import exercises.chapter3.{Branch, Leaf, Tree}

object Maximum extends App {

  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(a) => a
    case Branch(left, right) => maximum(left) max maximum(right)
  }



  println(maximum(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))))

}
