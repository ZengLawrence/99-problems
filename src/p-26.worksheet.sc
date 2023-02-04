// P26 (**) Generate the combinations of K distinct objects chosen from the N elements of a list.
def combinations[A](k: Int, ls: List[A]): List[List[A]] = (k, ls) match {
  case (_, Nil) => Nil
  case (1, ls) => ls.map(List(_))
  case (n, h :: t) => combinations(k - 1, t).map(h +: _) ++: combinations(k, t)
}

combinations(1, List('a', 'b', 'c'))

combinations(2, List('a', 'b', 'c'))

combinations(2, List('a', 'b', 'c', 'd'))

combinations(3, List('a', 'b', 'c', 'd'))

val c = combinations(3, List('a', 'b', 'c', 'd', 'e', 'f'))
c.length
assert(c.length == 20)