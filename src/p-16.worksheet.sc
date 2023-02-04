// P16 (**) Drop every Nth element from a list.
def drop[A](n: Int, l: List[A]): List[A] = 
  l.zipWithIndex.filter{(_, i) => (i + 1) % n > 0}.map(_._1)

val d = drop(3, List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k'))
assert(d == List('a', 'b', 'd', 'e', 'g', 'h', 'j', 'k'))

// comprehension implementation
def dropC[A](n: Int, l: List[A]): List[A] = 
  for {
    (a, i) <- l.zipWithIndex
    if ((i + 1) % n > 0)
  } yield {
    a
  }

val dc = dropC(3, List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k'))
assert(dc == List('a', 'b', 'd', 'e', 'g', 'h', 'j', 'k'))

// recursion
def dropR[A](n: Int, l: List[A]): List[A] = 
  @annotation.tailrec
  def go(c: Int, dl: List[A], l: List[A]): List[A] = (c, l) match {
    case (_, Nil) => dl
    case (1, h :: t) => go(n, dl, t)
    case (_, h :: t) => go(c - 1, h +: dl, t)
  }
  go(n, List[A](), l).reverse

val dr = dropR(3, List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k'))
assert(dr == List('a', 'b', 'd', 'e', 'g', 'h', 'j', 'k'))
