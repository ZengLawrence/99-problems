// P61A (*) Collect the leaves of a binary tree in a list.

sealed abstract class Tree[+T] {
  def leafCount = leafList.length
  def leafList: List[T]
}

case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {

  override def leafList: List[T] = (left, right) match {
    case (End, End) => List(value)
    case _ => left.leafList ++: right.leafList
  }

  override def toString = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"
}

case object End extends Tree[Nothing] {
  override def leafList: List[Nothing] = List()

  override def toString = "."
}

object Node {
  def apply[T](value: T): Node[T] = Node(value, End, End)
}

assert(Node('a', Node('b'), Node('c', Node('d'), Node('e'))).leafList == List('b', 'd', 'e'))
assert(Node('a', Node('b'), Node('c', Node('d'), Node('e'))).leafCount == 3)