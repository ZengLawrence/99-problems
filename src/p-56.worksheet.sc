// P56 (**) Symmetric binary trees.

sealed abstract class Tree[+T] {
  def isMirrorOf[T](tree: Tree[T]): Boolean
  def isSymmetric: Boolean
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

assert(Node('a', Node('b'), Node('c')).isSymmetric == true)
assert(Node('a', Node('b'), End).isSymmetric == false)
assert(Node('a', Node('b', Node('d'), End), Node('c', End, Node('e'))).isSymmetric == true)
assert(Node('a', Node('b', Node('d'), Node('f')), Node('c', Node('g'), Node('e'))).isSymmetric == true)
assert(Node('a', Node('b', Node('d'), End), Node('c', Node('g'), Node('e'))).isSymmetric == false)
assert(Node('a', Node('b', Node('d'), Node('f')), Node('c', Node('g'), End)).isSymmetric == false)
assert(End.isSymmetric == true)