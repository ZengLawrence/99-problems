//P62B (*) Collect the nodes at a given level in a list.

sealed abstract class Tree[+T] {
  def atLevel(level: Int): List[T]
}

case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {

  override def atLevel(level: Int): List[T] = level match {
    case _ if level < 1 => Nil
    case 1 => List(value)
    case _ => left.atLevel(level - 1) ::: right.atLevel(level - 1)
  }

  override def toString = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"
}

case object End extends Tree[Nothing] {

  override def atLevel(level: Int): List[Nothing] = List()

  override def toString = "."
}

object Node {
  def apply[T](value: T): Node[T] = Node(value, End, End)
}

assert(Node('a', Node('b'), Node('c', Node('d'), Node('e'))).atLevel(2) == List('b', 'c'))