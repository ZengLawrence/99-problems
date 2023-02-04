// Run-length encoding of a list.
//import P09.pack

def pack[A](l: List[A]): List[List[A]] =
  @annotation.tailrec
  def go(pl: List[List[A]], l: List[A]): List[List[A]] = l match
    case Nil => pl
    case h :: _ => {
      val (nl, t) = l.span(_ == h)
      go(nl +: pl, t)
    }
    go(List[List[A]](), l).reverse


def encode[A](l: List[A]): List[(Int, A)] =
  pack(l).map(al => (al.length, al.head))

val e = encode(List("a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e"))
assert(e == List((4,"a"), (1,"b"), (2,"c"), (2,"a"), (1,"d"), (4,"e")))