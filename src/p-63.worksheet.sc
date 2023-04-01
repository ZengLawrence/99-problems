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
    def go(nodeNumber: Int): Tree[T] = nodeNumber match {
      case n if n <= nodes => Node(value, go(2 * n), go(2 * n + 1))
      case _ => End
    }
    go(1)
}

assert(Tree.completeBinaryTree(6, "x") == Node("x", 
                                                Node("x", Node("x", End, End), Node("x", End, End)), 
                                                Node("x", Node("x", End, End), End)))