//Modified run-length encoding.
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

def encodeModified[A](l: List[A]): List[(Int, A) | A] =
  pack(l).map(pl => {
    if pl.length == 1 then pl.head
    else (pl.length, pl.head)
  })

val e = encodeModified(List("a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e"))
assert(e == List((4,"a"), "b", (2,"c"), (2,"a"), "d", (4,"e")))
