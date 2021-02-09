package exercises.chapter3

import scala.annotation.tailrec

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree extends App {

  def size(tree: Tree[_]): Int = tree match {
    case Leaf(_) => 1
    case Branch(left, right) => size(left) + size(right) + 1
  }

  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(a) => a
    case Branch(left, right) => maximum(left) max maximum(right)
  }

  def depth(tree: Tree[_]): Int = tree match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + (depth(left) max depth(right))
  }

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(a) => Leaf(f(a))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  def fold[A, B](tree: Tree[A])(f: A => B)(g: (B, B) => B): B = tree match {
    case Leaf(a) => f(a)
    case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
  }

  def foldSize(tree: Tree[_]): Int = fold(tree)(_ => 1)((left, right) => left + right + 1)
  def foldMaximum(tree: Tree[Int]): Int = fold(tree)(identity)(_ max _)
  def foldDepth(tree: Tree[_]): Int = fold(tree)(_ => 1)((left, right) => (left max right) + 1)
  def foldMap[A, B](tree: Tree[A])(f: A => B): Tree[B] = fold(tree)(left => Leaf(f(left)): Tree[B])((left, right) => Branch(left, right))

  println(depth(Branch(Leaf(5), Branch(Leaf(2), Leaf(4)))))

}