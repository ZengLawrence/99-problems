// P56 (**) Symmetric binary trees.

sealed abstract class Tree[+T] {
  def isMirrorOf[T](tree: Tree[T]): Boolean = false
}


case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
  override def toString = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"
  override def isMirrorOf[T](tree: Tree[T]): Boolean = tree match {
    case Node(_, left, right) => this.left.isMirrorOf(right) && this.right.isMirrorOf(left)
    case End => false
  }
    
  def isSymmetric: Boolean = this.left.isMirrorOf(this.right)
}

case object End extends Tree[Nothing] {
  override def toString = "."
  override def isMirrorOf[T](tree: Tree[T]): Boolean = tree match {
    case End => true
    case _ => false
  }
}

object Node {
  def apply[T](value: T): Node[T] = Node(value, End, End)
}

end Tree

assert(Node('a', Node('b'), Node('c')).isSymmetric == true)
assert(Node('a', Node('b'), End).isSymmetric == false)
assert(Node('a', Node('b', Node('d'), End), Node('c', End, Node('e'))).isSymmetric == true)
assert(Node('a', Node('b', Node('d'), Node('f')), Node('c', Node('g'), Node('e'))).isSymmetric == true)
assert(Node('a', Node('b', Node('d'), End), Node('c', Node('g'), Node('e'))).isSymmetric == false)
assert(Node('a', Node('b', Node('d'), Node('f')), Node('c', Node('g'), End)).isSymmetric == false)