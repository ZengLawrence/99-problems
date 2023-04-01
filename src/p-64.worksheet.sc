// P64 (**) Layout a binary tree (1).

sealed abstract class Tree[+T] {
  def inOrder: List[Tree[T]]
}

case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {

  override def inOrder: List[Tree[T]] = left.inOrder ::: List(this) ::: right.inOrder

  def layoutBinaryTree: Tree[T] =
    val inOrderMap = this.inOrder.zipWithIndex.groupMapReduce((t, _) => t){(_, i) => i + 1}{(_, i) => i}
    def lbt(tree: Tree[T], y: Int): Tree[T] = tree match {
      case End => End
      case Node(v, l, r): Node[T] => PositionedNode(v, lbt(l, y + 1), lbt(r, y + 1), inOrderMap(tree), y)
      case t => t
    }
    lbt(this, 1)

  override def toString = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"
}

case object End extends Tree[Nothing] {

  override def inOrder: List[Tree[Nothing]] = Nil

  override def toString = "."
}

object Node {
  def apply[T](value: T): Node[T] = Node(value, End, End)
}

case class PositionedNode[+T](val value: T, val left: Tree[T], val right: Tree[T], x: Int, y: Int) extends Tree[T] {

  override def inOrder: List[Tree[T]] = left.inOrder ::: List(this) ::: right.inOrder

  override def toString = "T[" + x.toString + "," + y.toString + "](" + value.toString + " " + left.toString + " " + right.toString + ")"
}

object Tree {
  def insert(tree: Tree[Char], value: Char): Tree[Char] = tree match {
    case End => Node(value)
    case Node(v, l, r) => 
      if (value < v) then Node(v, insert(l, value), r)
      else Node(v, l, insert(r, value))
  }

  def fromList(list: List[Char]): Node[Char] = 
    list.foldLeft(End: Tree[Char]){(t, v) => insert(t, v)} match {
      case n: Node[Char] => n
      case _ => throw new IllegalStateException("Unexpected type")
    }
}

Node('a', Node('b', End, Node('c')), Node('d')).layoutBinaryTree
//T[3,1](a T[1,2](b . T[2,3](c . .)) T[4,2](d . .))

val t = Tree.fromList(List('n', 'k', 'm', 'c', 'a', 'h', 'g', 'e', 'u', 'p', 's', 'q'))
t.layoutBinaryTree
//T[8,1](n T[6,2](k T[2,3](c T[1,4](a . .) T[5,4](h T[4,5](g T[3,6](e . .) .) .)) T[7,3](m . .)) T[12,2](u T[9,3](p . T[11,4](s T[10,5](q . .) .)) .))