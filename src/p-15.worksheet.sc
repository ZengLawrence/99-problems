// P15 (**) Duplicate the elements of a list a given number of times.
def duplicateN[A](n: Int, l: List[A]): List[A] =
  l flatMap {a => List.fill(n)(a)}

val d = duplicateN(3, List("a", "b", "c", "c", "d"))
assert(d == List("a","a","a", "b", "b","b", "c", "c", "c", "c","c", "c", "d", "d", "d"))
