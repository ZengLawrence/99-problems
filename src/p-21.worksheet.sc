// P21 (*) Insert an element at a given position into a list.
def insertAt[A](a: A, i: Int, ls: List[A]): List[A] = ls.splitAt(i) match {
  case (pre, post) => pre ++: (a +: post)
}

val i = insertAt("new", 1, List("a", "b", "c", "d"))
assert(i == List("a", "new", "b", "c", "d"))

val i0 = insertAt("new", 0, List("a", "b", "c", "d"))
assert(i0 == List("new", "a", "b", "c", "d"))

insertAt("new", -1, List("a", "b", "c", "d"))
insertAt("new", 6, List("a", "b", "c", "d"))