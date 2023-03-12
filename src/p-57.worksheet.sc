// P57 (**) Binary search trees (dictionaries).

sealed abstract class Tree[+T] {
  def isMirrorOf[T](tree: Tree[T]): Boolean
  def isSymmetric: Boolean
  def addValue[U >: T](x: U)(implicit ev: U => Ordered[U]): Tree[U]
}

object Tree {
  def fromList[V](list: List[V])(implicit ev: V => Ordered[V]): Tree[V] = 
    list.foldLeft(End: Tree[V]){(t, v) => t.addValue(v)}
}

case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
  override def toString = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"
  override def isMirrorOf[T](tree: Tree[T]): Boolean = tree match {
    case t: Node[T] => left.isMirrorOf(t.right) && right.isMirrorOf(t.left)
    case End => false
  }
  def isSymmetric: Boolean = left.isMirrorOf(right)
  override def addValue[U >: T](x: U)(implicit ev: U => Ordered[U]): Tree[U] = 
    // explicit conversion using ev
    if ev(x) < value then Node(value, left.addValue(x), right)
    else Node(value, left, right.addValue(x))
}

case object End extends Tree[Nothing] {
  override def toString = "."
  override def isMirrorOf[T](tree: Tree[T]): Boolean = tree match {
    case End => true
    case _ => false
  }
  override def isSymmetric: Boolean = true
  override def addValue[U](x: U)(implicit ev: U => Ordered[U]): Tree[U] = Node(x, End, End)
}

object Node {
  def apply[T](value: T): Node[T] = Node(value, End, End)
}

val r1 = End.addValue(2)
assert(r1 == Node(2, End, End))
val r2 = r1.addValue(3)
assert(r2 == Node(2, End, Node(3, End, End)))
val r3 = r2.addValue(0)
assert(r3 == Node(2, Node(0, End, End), Node(3, End, End)))

Tree.fromList(List(3, 2, 5, 7, 1))
assert(Tree.fromList(List(5, 3, 18, 1, 4, 12, 21)).isSymmetric == true)
assert(Tree.fromList(List(3, 2, 5, 7, 4)).isSymmetric == false)