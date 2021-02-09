package exercises.chapter3.twentySeventh

import exercises.chapter3.{Branch, Leaf, Tree}

object Depth extends App {

  def depth(tree: Tree[_]): Int = tree match {
    case Leaf(_) => 1
//    case Branch(left, right) => depth()
  }

}
