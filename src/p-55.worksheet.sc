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

object Tree {

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