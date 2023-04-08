// P66 (***) Layout a binary tree (3).

// Not a working solution

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
  def leftMostNodeDepth: Int
}

case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {

  override def successorDepth(depth: Int): Int = right.leftMostNodeDepth + 1

  override def leftMostNodeDepth: Int = left.leftMostNodeDepth + 1

  override def layoutBinaryTree3Internal(x: Int, depth: Int): (Tree[T], Map[Int, Int]) = 
    val lx = if x > 1 then x - 1 else x
    val (l, ldm) = left.layoutBinaryTree3Internal(lx, depth + 1)
    val myX = (ldm.get(depth + 1), ldm.get(successorDepth(depth))) match {
      case (None, None) => x
      case (None, Some(successorX))=> successorX + 1
      case (Some(childX), None) => childX + 1
      case (Some(childX), Some(successorX)) => Math.max(childX, successorX) + 1
    }
    val rx = ldm.get(depth + 1) match {
      case None => myX + 1
      case Some(lcx) => myX + (myX - lcx)
    }
    val (r, rdm) = right.layoutBinaryTree3Internal(rx, depth + 1)
    (PositionedNode(value, l, r, myX, depth), (ldm ++ rdm + (depth -> myX)))

  override def toString = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"
}

case object End extends Tree[Nothing] {

  override def successorDepth(depth: Int): Int = 0

  override def leftMostNodeDepth: Int = 0

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
assert(n.right.leftMostNodeDepth == 1)
assert(n.successorDepth(1) == 2)
n.layoutBinaryTree3
//T[2,1]('a T[1,2]('b . T[2,3]('c . .)) T[3,2]('d . .))

val t = Tree.fromList(List('n', 'k', 'm', 'c', 'a', 'e', 'd', 'g', 'u', 'p', 'q')).get
t.layoutBinaryTree3
//T[4,1](n T[3,2](k T[2,3](c T[1,4](a . .) T[3,4](e T[2,5](d . .) T[4,5](g . .))) T[4,3](m . .)) T[7,2](u T[6,3](p . T[7,4](q . .)) .))

assert(t.successorDepth(1) == 3)

val c = Node('c', Node('a', End, Node('b')), Node('f', Node('e', Node('d'), End), End))
c.layoutBinaryTree3
//T[3,1](c T[1,2](a . T[2,3](b . .)) T[5,2](f T[4,3](e T[3,4](d . .) .) .))
