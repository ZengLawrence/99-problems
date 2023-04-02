// P64 (**) Layout a binary tree (1).

sealed abstract class Tree[+T] {
  def layoutBinaryTree: Tree[T] = layoutBinaryTreeInternal(1, 1)._1
  def layoutBinaryTreeInternal(x: Int, y: Int): (Tree[T], Int)
}

case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {

  override def layoutBinaryTreeInternal(x: Int, y: Int): (Tree[T], Int) = 
    val (l, myX) = left.layoutBinaryTreeInternal(x, y + 1)
    val (r, nextX) = right.layoutBinaryTreeInternal(myX + 1, y + 1)
    (PositionedNode(value, l, r, myX, y), nextX)

  override def toString = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"
}

case object End extends Tree[Nothing] {

  override def layoutBinaryTreeInternal(x: Int, y: Int): (Tree[Nothing], Int) = (End, x)

  override def toString = "."
}

object Node {
  def apply[T](value: T): Node[T] = Node(value, End, End)
}


class PositionedNode[+T](override val value: T, override val left: Tree[T], override val right: Tree[T], val x: Int, val y: Int) extends Node(value, left, right) {

  override def toString = "T[" + x.toString + "," + y.toString + "](" + value.toString + " " + left.toString + " " + right.toString + ")"
}

object Tree {
  def insert[V](tree: Tree[V], value: V)(implicit ev: V => Ordered[V]): Node[V] = tree match {
    case End => Node(value)
    case Node(v, l, r) => 
      if (ev(value) < v) then Node(v, insert(l, value), r)
      else Node(v, l, insert(r, value))
  }

  def fromList[V](list: List[V])(implicit ev: V => Ordered[V]): Option[Node[V]] = 
    list.foldLeft(End: Tree[V]){(t, v) => insert(t, v)} match {
      case n: Node[V] => Some(n)
      case _ => None
    }
}

Node('a', Node('b', End, Node('c')), Node('d')).layoutBinaryTree
//T[3,1](a T[1,2](b . T[2,3](c . .)) T[4,2](d . .))

val t = Tree.fromList(List('n', 'k', 'm', 'c', 'a', 'h', 'g', 'e', 'u', 'p', 's', 'q')).get
t.layoutBinaryTree
//T[8,1](n T[6,2](k T[2,3](c T[1,4](a . .) T[5,4](h T[4,5](g T[3,6](e . .) .) .)) T[7,3](m . .)) T[12,2](u T[9,3](p . T[11,4](s T[10,5](q . .) .)) .))