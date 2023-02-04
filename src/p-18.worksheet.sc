// P18 (**) Extract a slice from a list.
def slice[A](i: Int, k: Int, ls: List[A]): List[A] = 
  ls.slice(i, k)

val s = slice(3, 7, List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k'))
assert(s == List('d', 'e', 'f', 'g'))

// recursion
def sliceR[A](start: Int, until: Int, ls: List[A]): List[A] = 
  @annotation.tailrec
  def go(i: Int, slides: List[A], ls: List[A]): List[A] = (i, ls) match {
    case (_, Nil) => slides
    case (i, _) if i >= until => slides
    case (i, _ :: t) if i < start => go(i + 1, slides, t)
    case (i, h :: t) => go(i + 1, h +: slides, t)
  }
  go(0, List[A](), ls).reverse

val sr = sliceR(3, 7, List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k'))
assert(sr == List('d', 'e', 'f', 'g'))
