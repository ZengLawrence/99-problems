// P62 (*) Collect the internal nodes of a binary tree in a list.

sealed abstract class Tree[+T] {
  def internalList: List[T]
}

case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {

  override def internalList: List[T] = (left, right) match {
    case (End, End) => List()
    case _ => value +: (left.internalList ::: right.internalList)
  }

  override def toString = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"
}

case object End extends Tree[Nothing] {

  override def internalList: List[Nothing] = List()

  override def toString = "."
}

object Node {
  def apply[T](value: T): Node[T] = Node(value, End, End)
}

assert(Node('a', Node('b'), Node('c', Node('d'), Node('e'))).internalList == List('a', 'c'))