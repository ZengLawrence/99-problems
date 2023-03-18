/**
 * P59 (**) Construct height-balanced binary trees.

In a height-balanced binary tree, the following property holds for every node: The height of its left subtree and the height of its right subtree are almost equal, which means their difference is not greater than one.

Write a method Tree.hbalTrees to construct height-balanced binary trees for a given height with a supplied value for the nodes.  The function should generate all solutions.
*/

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

// start implementation
object Tree {

  def hbalTrees[V](height: Int, value: V): List[Tree[V]] = height match {
    case 0 => List(End)
    case 1 => List(Node(value))
    case n if n > 1 => 
      val balHeightTrees = 
        for 
          l <- hbalTrees(n - 1, value)
          r <- hbalTrees(n - 1, value)
        yield Node(value, l, r)
      val unbalHeightTrees = for 
        lst <- hbalTrees(n - 2, value)
        gst <- hbalTrees(n - 1, value)
        node <- List(Node(value, lst, gst), Node(value, gst, lst))
      yield node
      balHeightTrees ++: unbalHeightTrees
  }

}

assert(Tree.hbalTrees(1, 'x') == List(Node('x', End, End)))
assert(Tree.hbalTrees(2, 'x').toSet == List(Node('x', Node('x'), End), 
                                            Node('x', End, Node('x')), 
                                            Node('x', Node('x'), Node('x'))).toSet)
assert(Tree.hbalTrees(3, "x").length == 15)