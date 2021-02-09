package exercises.chapter3.twentyFifth

import exercises.chapter3.{Branch, Leaf, Tree}

object SizeTree extends App {

  def size(tree: Tree[_]): Int = tree match {
    case Leaf(_) => 1
    case Branch(left, right) => size(left) + size(right) + 1
  }

}
