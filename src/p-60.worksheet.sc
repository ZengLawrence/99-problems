// P60 (**) Construct height-balanced binary trees with a given number of nodes.

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

// helper functions
def minHbalNodes(height: Int): Int = height match {
  case 1 => 1
  case 2 => 2
  case h => 1 + minHbalNodes(h - 1) + minHbalNodes(h - 2)
}

assert(minHbalNodes(3) == 4)

def maxHbalHeight(nodes: Int): Int =
  def go(nodes: Int, height: Int): Int =
    val minNodes = minHbalNodes(height)
    if nodes < minNodes then height - 1
    else go(nodes, height + 1)
  go(nodes, 1)

assert(maxHbalHeight(4) == 3)

object Tree {

  def hbalTrees[V](height: Int, value: V): List[Tree[V]] = 
    def sameHeightTrees(subtrees: List[Tree[V]]) =
      for 
        l <- subtrees
        r <- subtrees
      yield Node(value, l, r)
    def unevenHeightTrees(shortSubTrees: List[Tree[V]], fullHeightSubTrees: List[Tree[V]]) =
      for 
        sst <- shortSubTrees
        fst <- fullHeightSubTrees
        node <- List(Node(value, sst, fst), Node(value, fst, sst))
      yield node
    height match {
      case 0 => List(End)
      case 1 => List(Node(value))
      case h if h > 1 => 
        val fullHeightSubTrees = hbalTrees(h - 1, value)
        val shortSubTrees = hbalTrees(h - 2, value)
        sameHeightTrees(fullHeightSubTrees) ++: unevenHeightTrees(shortSubTrees, fullHeightSubTrees)
    }

  def nodeCount[V](tree: Tree[V]): Int = tree match {
    case End => 0
    case Node(_, l, r) => 1 + nodeCount(l) + nodeCount(r)
  }
    
  // implementation
  def hbalTreesWithNodes[V](nodes: Int, value: V): List[Tree[V]] =
    {for 
      h <- 1 to maxHbalHeight(nodes) // should find a lower bound to make it slightly more efficient
      t <- hbalTrees(h, value)
      if nodeCount(t) == nodes
    yield t}.toList

}

Tree.hbalTreesWithNodes(4, "x")