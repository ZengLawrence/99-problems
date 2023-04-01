// P64 (**) Layout a binary tree (1).

sealed abstract class Tree[+T]

trait Node[+T] {
  val value: T
  val left: Tree[T]
  val right: Tree[T]
  def layoutBinaryTree: Tree[T]
}

case class RegularNode[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
  override def toString = "T(" + value.toString + " " + left.toString + " " + right.toString + ")"
}

case object End extends Tree[Nothing] {
  override def toString = "."
}

object Node {
  def apply[T](value: T): RegularNode[T] = RegularNode(value, End, End)
}

case class PositionedNode[+T](override val value: T, override val left: Tree[T], override val right: Tree[T], x: Int, y: Int) extends Tree[T], Node[T] {

  override def layoutBinaryTree: Tree[T] = ???
  
  override def toString = "T[" + x.toString + "," + y.toString + "](" + value.toString + " " + left.toString + " " + right.toString + ")"
}

