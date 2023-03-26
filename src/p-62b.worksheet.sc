//P62B (*) Collect the nodes at a given level in a list.

sealed abstract class Tree[+T] {
  def atLevel(level: Int): List[T]
}

case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {

  override def atLevel(level: Int): List[T] = 
    def go(tree: Tree[T], currLevel: Int): List[T] = tree match {
      case Node(v, _, _) if currLevel == level => List(v)
      case Node(_, l, r) if currLevel < level => go(l, currLevel + 1) ::: go(r, currLevel + 1)
      case _ => List()
    }
    go(this, 1)

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