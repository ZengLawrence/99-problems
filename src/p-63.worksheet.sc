// P63 (**) Construct a complete binary tree.

sealed abstract class Tree[+T]

case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
  override def toString = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"
}

case object End extends Tree[Nothing] {
  override def toString = "."
}

object Node {
  def apply[T](value: T): Node[T] = Node(value, End, End)
}

object Tree {
  def completeBinaryTree[T](nodes: Int, value: T): Tree[T] =
    def go(i: Int): Tree[T] = 
      if i > nodes then End
      else Node(value, go(2 * i), go(2 * i + 1))
    go(1)
}

assert(Tree.completeBinaryTree(6, "x") == Node("x", 
                                                Node("x", Node("x", End, End), Node("x", End, End)), 
                                                Node("x", Node("x", End, End), End)))