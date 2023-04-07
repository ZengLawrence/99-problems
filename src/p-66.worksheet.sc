// P66 (***) Layout a binary tree (3).

/**
    * each node returns map of depth to right most node x
    * a parent node finds level of its successor
    * finds max of x of left child or successor level, if found, max + 1
    * if both not found, use x
    * this is x for parent node
    */
sealed abstract class Tree[+T] {
  def layoutBinaryTree3: Tree[T] =
    layoutBinaryTree3Internal(1, 1)._1
  def layoutBinaryTree3Internal(x: Int, depth: Int): (Tree[T], Map[Int, Int])
  def successorDepth(depth: Int): Int
}

case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {

  override def successorDepth(depth: Int): Int = Math.max(depth, right.successorDepth(depth + 1))

  override def layoutBinaryTree3Internal(x: Int, depth: Int): (Tree[T], Map[Int, Int]) = 
    val (l, ldm) = left.layoutBinaryTree3Internal(x, depth + 1)
    val myX = (ldm.get(depth + 1), ldm.get(successorDepth(depth))) match {
      case (None, None) => x
      case (None, Some(successorX))=> successorX + 1
      case (Some(childX), None) => childX + 1
      case (Some(childX), Some(successorX)) => Math.max(childX, successorX) + 1
    }
    val (r, rdm) = right.layoutBinaryTree3Internal(myX + 1, depth + 1)
    (PositionedNode(value, l, r, myX, depth), (ldm ++ rdm + (depth -> myX)))

  override def toString = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"
}

case object End extends Tree[Nothing] {

  override def successorDepth(depth: Int): Int = depth - 1


  override def layoutBinaryTree3Internal(x: Int, depth: Int): (Tree[Nothing], Map[Int, Int]) = (End, Map.empty)

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

val n = Node('a', Node('b', End, Node('c')), Node('d'))
n.successorDepth(1)
n.layoutBinaryTree3
//T[2,1]('a T[1,2]('b . T[2,3]('c . .)) T[3,2]('d . .))