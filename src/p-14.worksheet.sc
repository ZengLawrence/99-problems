// P14 (*) Duplicate the elements of a list.
def duplicate[A](l: List[A]): List[A] =
  l flatMap {a => List(a, a)}

val d = duplicate(List("a", "b", "c", "c", "d"))
assert(d == List("a","a", "b", "b", "c", "c", "c", "c", "d", "d"))

def duplicateR[A](l: List[A]): List[A] =
  def go (dl: List[A], l: List[A]): List[A] = l match {
    case Nil => dl
    case h :: t => go(h +: h +: dl, t)
  }
  go(List[A](), l).reverse

val dr = duplicateR(List("a", "b", "c", "c", "d"))
assert(dr == List("a","a", "b", "b", "c", "c", "c", "c", "d", "d"))
