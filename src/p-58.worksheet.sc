//P58 (**) Generate-and-test paradigm.

sealed abstract class Tree[+T] {
  def isMirrorOf[T](tree: Tree[T]): Boolean
  def isSymmetric: Boolean
}

object Tree {

  // better recursive version
  def cBalanced[T](nodes: Int, value: T): List[Tree[T]] = nodes match {
    case n if n < 1 => List(End)
    case n if n % 2 == 1 => 
      for 
        l <- cBalanced((n - 1) / 2, value)
        r <- cBalanced((n - 1) / 2, value)
      yield Node(value, l, r)
    case n if n % 2 == 0 => 
      for 
        l <- cBalanced((n - 1) / 2, value)
        g <- cBalanced(((n - 1) / 2) + 1, value)
        node <- List(Node(value, l, g), Node(value, g, l))
      yield node
  }

  def symmetricBalancedTrees[T](nodes: Int, value: T): List[Tree[T]] =
    cBalanced(nodes, value) filter {_.isSymmetric}

}

case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
  override def toString = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"
  override def isMirrorOf[T](tree: Tree[T]): Boolean = tree match {
    case t: Node[T] => left.isMirrorOf(t.right) && right.isMirrorOf(t.left)
    case End => false
  }
    
  def isSymmetric: Boolean = left.isMirrorOf(right)
}

case object End extends Tree[Nothing] {
  override def toString = "."
  override def isMirrorOf[T](tree: Tree[T]): Boolean = tree match {
    case End => true
    case _ => false
  }
  override def isSymmetric: Boolean = true
}

object Node {
  def apply[T](value: T): Node[T] = Node(value, End, End)
}

Tree.symmetricBalancedTrees(5, "x")
assert(Tree.symmetricBalancedTrees(5, "x") == List(Node("x", Node("x", End, Node("x", End, End)), Node("x", Node("x", End, End), End)), 
                                                  Node("x", Node("x", Node("x", End, End), End), Node("x", End, Node("x", End, End)))))