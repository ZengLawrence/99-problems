// P20 (*) Remove the Kth element from a list.
def removeAt[A](i: Int, ls: List[A]): (List[A], A) = ls.splitAt(i) match {
  case (Nil, _) if i < 0 => throw new NoSuchElementException
  case (prefix, h :: suffix) => (prefix ++: suffix, h)
  case (_, Nil) => throw new NoSuchElementException
}

val r = removeAt(1, List('a', 'b', 'c', 'd'))
assert(r == (List('a', 'c', 'd'), 'b'))

val r0 = removeAt(0, List('a', 'b', 'c', 'd'))
assert(r0 == (List('b', 'c', 'd'), 'a'))

// removeAt(6, List('a', 'b', 'c', 'd')) // NoSuchElementException
// removeAt(-1, List('a', 'b', 'c', 'd')) // NoSuchElementException
