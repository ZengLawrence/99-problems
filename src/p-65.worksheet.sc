// P65 (**) Layout a binary tree (2).

sealed abstract class Tree[+T] {
  def maxDepth: Int
  def layoutBinaryTree2: Tree[T] = layoutBinaryTree2Internal(1, Math.pow(2, maxDepth - 1).toInt, 1)._1
  def layoutBinaryTree2Internal(x: Int, hDistance: Int, y: Int): (Tree[T], Int)
}

case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {

  override def layoutBinaryTree2Internal(x: Int, hDistance: Int, y: Int): (Tree[T], Int) = 
    val (l, myX) = left.layoutBinaryTree2Internal(x, hDistance / 2, y + 1)
    val (r, _) = right.layoutBinaryTree2Internal(myX + hDistance / 2, hDistance / 2, y + 1)
    (PositionedNode(value, l, r, myX, y), myX + hDistance)

  override def maxDepth: Int = Math.max(left.maxDepth, right.maxDepth) + 1

  override def toString = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"
}

case object End extends Tree[Nothing] {

  override def maxDepth: Int = 0

  override def layoutBinaryTree2Internal(x: Int, hDistance: Int, y: Int): (Tree[Nothing], Int) = (End, x)

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

Node('a', Node('b', End, Node('c')), Node('d')).layoutBinaryTree2
//T[3,1]('a T[1,2]('b . T[2,3]('c . .)) T[5,2]('d . .))