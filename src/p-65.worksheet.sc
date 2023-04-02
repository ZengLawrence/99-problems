// P65 (**) Layout a binary tree (2).

// The layout rules for a node v with parent u and depth d are as follows:
// * x(v) is x(u) plus or minus 2^(m-d), where m is the maximum depth of the
//   tree.  The leftmost node has x(v) == 1.
// * y(v) == d
sealed abstract class Tree[+T] {
  def maxDepth: Int
  def leftMostNodeDepth: Int
  def layoutBinaryTree2: Tree[T] = 
    val md = maxDepth
    val x0 = (2 to leftMostNodeDepth).map(d => Math.pow(2, md - d).toInt).reduceLeft(_ + _) + 1
    layoutBinaryTree2Internal(x0, 1, md - 2)

  def layoutBinaryTree2Internal(x0: Int, depth: Int, exp: Int): Tree[T]
}

case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {

  override def layoutBinaryTree2Internal(x0: Int, depth: Int, exp: Int): Tree[T] = 
    PositionedNode(
      value,
      left.layoutBinaryTree2Internal(x0 - Math.pow(2, exp).toInt, depth + 1, exp - 1),
      right.layoutBinaryTree2Internal(x0 + Math.pow(2, exp).toInt, depth + 1, exp - 1),
      x0,
      depth
      )

  override def leftMostNodeDepth: Int = left.leftMostNodeDepth + 1
  override def maxDepth: Int = Math.max(left.maxDepth, right.maxDepth) + 1

  override def toString = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"
}

case object End extends Tree[Nothing] {

  override def layoutBinaryTree2Internal(x0: Int, depth: Int, exp: Int): Tree[Nothing] = End

  override def leftMostNodeDepth: Int = 0

  override def maxDepth: Int = 0

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

val t = Tree.fromList(List('n', 'k', 'm', 'c', 'a', 'e', 'd', 'g', 'u', 'p', 'q')).get
t.maxDepth
t.leftMostNodeDepth
t.layoutBinaryTree2
//T[15,1](n T[7,2](k T[3,3](c T[1,4](a . .) T[5,4](e T[4,5](d . .) T[6,5](g . .))) T[11,3](m . .)) T[23,2](u T[23,3](p . T[21,4](q . .)) .))