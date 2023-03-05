// P55 (**) Construct completely balanced binary trees.

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

end Tree

object Tree {

  // balanced tree level for given n. Zero based.
  // return tuple of (level, extra nodes on the last level)
  def level(n: Int): (Int, Int) =
    def go(level: Int, n: Int): (Int, Int) = 
      val node4Level = Math.pow(2, level).toInt
      if n > node4Level then go(level + 1, n - node4Level)
      else if n == node4Level then (level, 0)
      else (level, n)
    go(0, n)

  def cBalanced[T](n: Int, value: T): Tree[T] = n match {
    case n if n > 1 => {
      val nn = n - 1
      Node(value, cBalanced(nn / 2, value), cBalanced(nn - (nn / 2), value))
    }
    case 1 => Node(value)
    case _ => End
  }

  def cBalanced[T](nodes: Array[Boolean], value: T): Tree[T] =
    def go(i: Int, nodes: Array[Boolean], value: T): Tree[T] = 
      if i < nodes.length then
        if nodes(i) then Node(value, go((i * 2) + 1, nodes, value), go((i * 2) + 2, nodes, value))
        else End
      else
        End

    go(0, nodes, value)
}

Tree.cBalanced(Array(true), 'x')
Tree.cBalanced(Array(true, true), 'x')
Tree.cBalanced(Array(true, true, false), 'x')
Tree.cBalanced(Array(true, false, true), 'x')

Tree.cBalanced(7, 'x')

def nToNodeArray(n: Int): Array[Boolean] = 
  val (level, extra) = Tree.level(n)
  Array.fill(n - extra)(true)

// unit tests
nToNodeArray(1).toList

assert(Tree.level(1) == (0, 0))
assert(Tree.level(2) == (1, 1))
assert(Tree.level(3) == (1, 0))
assert(Tree.level(4) == (2, 1))
assert(Tree.level(5) == (2, 2))
assert(Tree.level(6) == (2, 3))
assert(Tree.level(7) == (2, 0))
assert(Tree.level(8) == (3, 1))
