// P17 (*) Split a list into two parts.
def split[A](n: Int, l: List[A]): (List[A], List[A]) =
  l.splitAt(n)

val s = split(3, List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k'))
assert(s == (List('a', 'b', 'c'), List('d', 'e', 'f', 'g', 'h', 'i', 'j', 'k')))

def splitR[A](n: Int, l: List[A]): (List[A], List[A]) =
  @annotation.tailrec
  def go(n: Int, prefix: List[A], l: List[A]): (List[A], List[A]) = (n, l) match {
    case (0, _) => (prefix.reverse, l)
    case (_, Nil) => (prefix.reverse, l)
    case (_, h :: t) => go(n - 1, h +: prefix, t)
  }
  go(n, List[A](), l)

val sr = splitR(3, List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k'))
assert(sr == (List('a', 'b', 'c'), List('d', 'e', 'f', 'g', 'h', 'i', 'j', 'k')))
