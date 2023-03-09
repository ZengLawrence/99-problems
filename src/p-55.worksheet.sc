// P55 (**) Construct completely balanced binary trees.
//     In a completely balanced binary tree, the following property holds for
//     every node: The number of nodes in its left subtree and the number of
//     nodes in its right subtree are almost equal, which means their difference
//     is not greater than one. 

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

// helper functions
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

  def cBalanced[T](n: Int, value: T): List[Tree[T]] = 
    def buildTree[T](i: Int, value: T)(isNode: (i: Int) => Boolean): Tree[T] =
      if isNode(i) then Node(value, buildTree((i*2) + 1, value){isNode}, buildTree((i*2) + 2, value){isNode})
      else End
    val (level, extra) = Tree.level(n)
    if (extra > 0) then 
      val lastLevelNodeStartIndex = startNodeIndex(level)
      val lastLevelNodeIndices = Range(lastLevelNodeStartIndex, startNodeIndex(level + 1)).toList
      combinations(extra, lastLevelNodeIndices) map {
        lastLevelNodes => buildTree(0, value){i => i < lastLevelNodeStartIndex || lastLevelNodes.contains(i)}
      }
    else 
      List(buildTree(0, value){i => i < startNodeIndex(level + 1)})

  // better recursive version
  def cBalanced2[T](nodes: Int, value: T): List[Tree[T]] = nodes match {
    case n if n < 1 => List(End)
    case n if n % 2 == 1 => {
      cBalanced2((n - 1) / 2, value) flatMap {
        l => cBalanced2((n - 1) / 2, value) flatMap {
          r => List(Node(value, l, r))
        }
      }
    }
    case n if n % 2 == 0 => {
      val lessSubTrees = cBalanced2((n - 1) / 2, value)
      val greaterSubTrees = cBalanced2(((n - 1) / 2) + 1, value)
      greaterSubTrees flatMap {
        g => lessSubTrees flatMap {
          l => List(Node(value, g, l), Node(value, l, g))
        }
      }
    }
  }
}

assert(Tree.cBalanced(1, 'x') == List(Node('x', End, End)))
assert(Tree.cBalanced(2, 'x') == List(Node('x', Node('x', End, End), End), 
                                      Node('x', End, Node('x', End, End))))
assert(Tree.cBalanced(3, 'x') == List(Node('x', Node('x', End, End), Node('x', End, End))))
assert(Tree.cBalanced(4, 'x') == List(Node('x', Node('x', Node('x', End, End), End), Node('x', End, End)), 
                                      Node('x', Node('x', End, Node('x', End, End)), Node('x', End, End)),
                                      Node('x', Node('x', End, End), Node('x', Node('x', End, End), End)),
                                      Node('x', Node('x', End, End), Node('x', End, Node('x', End, End)))))
// 5 nodes are incorrect. Some sub trees are more than 2 nodes larger.
assert(Tree.cBalanced(5, 'x') == List(Node('x', Node('x', Node('x', End, End), Node('x', End, End)), Node('x', End, End)),
                                      Node('x', Node('x', Node('x', End, End), End), Node('x', Node('x', End, End), End)),
                                      Node('x', Node('x', Node('x', End, End), End), Node('x', End, Node('x', End, End))),
                                      Node('x', Node('x', End, Node('x', End, End)), Node('x', Node('x', End, End), End)),
                                      Node('x', Node('x', End, Node('x', End, End)), Node('x', End, Node('x', End, End))),
                                      Node('x', Node('x', End, End), Node('x', Node('x', End, End), Node('x', End, End)))))
assert(Tree.cBalanced(6, 'x') == List(Node('x', Node('x', Node('x', End, End), Node('x', End, End)), Node('x', Node('x', End, End), End)),
                                      Node('x', Node('x', Node('x', End, End), Node('x', End, End)), Node('x', End, Node('x', End, End))),
                                      Node('x', Node('x', Node('x', End, End), End), Node('x', Node('x', End, End), Node('x', End, End))),
                                      Node('x', Node('x', End, Node('x', End, End)), Node('x', Node('x', End, End), Node('x', End, End)))))
assert(Tree.cBalanced(7, 'x') == List(Node('x', Node('x', Node('x', End, End), Node('x', End, End)), Node('x', Node('x', End, End), Node('x', End, End)))))


assert(Tree.level(1) == (0, 0))
assert(Tree.level(2) == (1, 1))
assert(Tree.level(3) == (1, 0))
assert(Tree.level(4) == (2, 1))
assert(Tree.level(5) == (2, 2))
assert(Tree.level(6) == (2, 3))
assert(Tree.level(7) == (2, 0))
assert(Tree.level(8) == (3, 1))

// recursive version
assert(Tree.cBalanced2(1, 'x') == List(Node('x', End, End)))
assert(Tree.cBalanced2(2, 'x') == List(Node('x', Node('x', End, End), End), 
                                      Node('x', End, Node('x', End, End))))
assert(Tree.cBalanced2(3, 'x') == List(Node('x', Node('x', End, End), Node('x', End, End))))
assert(Tree.cBalanced2(4, 'x').toSet == List(Node('x', Node('x', Node('x', End, End), End), Node('x', End, End)), 
                                      Node('x', Node('x', End, Node('x', End, End)), Node('x', End, End)),
                                      Node('x', Node('x', End, End), Node('x', Node('x', End, End), End)),
                                      Node('x', Node('x', End, End), Node('x', End, Node('x', End, End)))).toSet)
assert(Tree.cBalanced2(5, 'x').toSet == List(Node('x', Node('x', Node('x', End, End), End), Node('x', Node('x', End, End), End)),
                                      Node('x', Node('x', Node('x', End, End), End), Node('x', End, Node('x', End, End))),
                                      Node('x', Node('x', End, Node('x', End, End)), Node('x', Node('x', End, End), End)),
                                      Node('x', Node('x', End, Node('x', End, End)), Node('x', End, Node('x', End, End)))).toSet)
assert(Tree.cBalanced2(6, 'x').toSet == List(Node('x', Node('x', Node('x', End, End), Node('x', End, End)), Node('x', Node('x', End, End), End)),
                                      Node('x', Node('x', Node('x', End, End), Node('x', End, End)), Node('x', End, Node('x', End, End))),
                                      Node('x', Node('x', Node('x', End, End), End), Node('x', Node('x', End, End), Node('x', End, End))),
                                      Node('x', Node('x', End, Node('x', End, End)), Node('x', Node('x', End, End), Node('x', End, End)))).toSet)
assert(Tree.cBalanced2(7, 'x') == List(Node('x', Node('x', Node('x', End, End), Node('x', End, End)), Node('x', Node('x', End, End), Node('x', End, End)))))