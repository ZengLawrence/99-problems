// P26 (**) Generate the combinations of K distinct objects chosen from the N elements of a list.
def combinations[A](k: Int, ls: List[A]): List[List[A]] = 
  def go[A](k: Int, ls: List[A]): List[List[A]] = (k, ls) match {
    case (_, Nil) => Nil
    case (1, ls) => ls.map(List(_))
    case (k, h :: t) => go(k - 1, t).map(h +: _) ++: go(k, t)
  }
  if (k < 1 || k > ls.length) then List(Nil)
  else go(k, ls)

combinations(1, List('a', 'b', 'c'))

combinations(2, List('a', 'b', 'c'))

combinations(2, List('a', 'b', 'c', 'd'))

combinations(3, List('a', 'b', 'c', 'd'))

val c = combinations(3, List('a', 'b', 'c', 'd', 'e', 'f'))
assert(c.length == 20)

// special cases
assert(combinations(2, List('a')) == List(Nil))
assert(combinations(0, List('a', 'b', 'c', 'd', 'e', 'f')) == List(Nil))
assert(combinations(-1, List('a')) == List(Nil))
assert(combinations(0, List()) == List(Nil))
assert(combinations(1, List()) == List(Nil))
