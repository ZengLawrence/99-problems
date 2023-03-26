// P61 (*) Count the leaves of a binary tree.

sealed abstract class Tree[+T] {
  def leafCount: Int
}

case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {

  override def leafCount: Int = (left, right) match {
    case (End, End) => 1
    case _ => left.leafCount + right.leafCount
  }

  override def toString = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"
}

case object End extends Tree[Nothing] {

  override def leafCount: Int = 0

  override def toString = "."
}

object Node {
  def apply[T](value: T): Node[T] = Node(value, End, End)
}

assert(Node('x').leafCount == 1)
assert(Node('x', Node('x'), End).leafCount == 1)
assert(Node('x', Node('x'), Node('x')).leafCount == 2)