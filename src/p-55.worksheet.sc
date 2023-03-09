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

}

Tree.cBalanced(7, 'x')

def combinations[A](k: Int, ls: List[A]): List[List[A]] = (k, ls) match {
    case (_, Nil) => Nil
    case (1, ls) => ls.map(List(_))
    case (k, h :: t) => combinations(k - 1, t).map(h +: _) ++: combinations(k, t)
  }

def nodeNumber(level: Int): Int = 
  if level < 0 then 0
  else Math.pow(2, level).toInt + nodeNumber(level - 1)

// unit test
assert(nodeNumber(0) == 1)
assert(nodeNumber(1) == 3)
assert(nodeNumber(2) == 7)
assert(nodeNumber(3) == 15)

def startNodeIndex(level: Int): Int = 
  if level < 0 then 0
  else nodeNumber(level - 1)

// unit test
assert(startNodeIndex(0) == 0)
assert(startNodeIndex(1) == 1)
assert(startNodeIndex(2) == 3)
assert(startNodeIndex(3) == 7)

combinations(2, Range(startNodeIndex(2) + 1, startNodeIndex(3)).toList)

assert(Tree.level(1) == (0, 0))
assert(Tree.level(2) == (1, 1))
assert(Tree.level(3) == (1, 0))
assert(Tree.level(4) == (2, 1))
assert(Tree.level(5) == (2, 2))
assert(Tree.level(6) == (2, 3))
assert(Tree.level(7) == (2, 0))
assert(Tree.level(8) == (3, 1))
