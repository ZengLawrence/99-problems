// Run-length encoding of a list (direct solution).

def encodeDirect[A](l: List[A]): List[(Int, A)] =
  @annotation.tailrec
  def go(pl: List[(Int, A)], l: List[A]): List[(Int, A)] = l match {
    case Nil => pl
    case h :: _ => {
      val (hl, tl) = l span {_ == h}
      go((hl.length, h) +: pl, tl)
    }
  }
  go(List[(Int, A)](), l).reverse

val e = encodeDirect(List("a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e"))
assert(e == List((4,"a"), (1,"b"), (2,"c"), (2,"a"), (1,"d"), (4,"e")))

// more generic implementation
def pack[A, B](l: List[A])(f: List[A] => B): List[B] =
  @annotation.tailrec
  def go(pl: List[B], l: List[A]): List[B] = l match {
    case Nil => pl
    case h :: _ => {
      val (hl, tl) = l span {_ == h}
      go(f(hl) +: pl, tl)
    }
  }
  go(List[B](), l).reverse

def encodeDirectG[A](l: List[A]): List[(Int, A)] =
  pack(l){ repeat => (repeat.length, repeat.head)}

val eg = encodeDirectG(List("a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e"))
assert(eg == List((4,"a"), (1,"b"), (2,"c"), (2,"a"), (1,"d"), (4,"e")))