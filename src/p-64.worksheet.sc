// P64 (**) Layout a binary tree (1).

sealed abstract class Tree[+T] {
  def inOrder: List[Tree[T]]
}

case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {

  override def inOrder: List[Tree[T]] = left.inOrder ::: List(this) ::: right.inOrder

  override def toString = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"
}

case object End extends Tree[Nothing] {

  override def inOrder: List[Tree[Nothing]] = Nil

  override def toString = "."
}

object Node {
  def apply[T](value: T): Node[T] = Node(value, End, End)
}
