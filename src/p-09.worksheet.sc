// If a list contains repeated elements they should be placed in separate sublists.
def pack[A](l: List[A]): List[List[A]] =
  l.foldLeft(List[List[A]]()){ (pl, a) => pl match
    case Nil => List(List(a)) 
    case h :: t if h.head == a => (a +: h) +: t
    case _ => List(a) +: pl
  }.reverse

val pl = pack(List("a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e"))
assert(pl == List(List("a", "a", "a", "a"), List("b"), List("c", "c"), List("a", "a"), List("d"), List("e", "e", "e", "e")))

def packR[A](l: List[A]): List[List[A]] =
  @annotation.tailrec
  def go(pl: List[List[A]], l: List[A]): List[List[A]] = (pl, l) match
    case (_, Nil) => pl // terminating condition
    case (Nil, (h :: t)) => go(List(List(h)), t) // for first list
    case ((ll :: lt), (h :: t)) if (ll.head == h) => go((h +: ll) +: lt, t) // add to pack list
    case (_, (h :: t)) => go(List(h) +: pl, t) // start a new pack list
  
  go(List[List[A]](), l).reverse

val plR = packR(List("a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e"))
assert(plR == List(List("a", "a", "a", "a"), List("b"), List("c", "c"), List("a", "a"), List("d"), List("e", "e", "e", "e")))

// better functional implementation
// this one maintain insertion order
def packF[A](l: List[A]): List[List[A]] =
  @annotation.tailrec
  def go(pl: List[List[A]], l: List[A]): List[List[A]] = l match
    case Nil => pl
    case h :: _ => {
      val (nl, t) = l.span(_ == h)
      go(nl +: pl, t)
    }
  
    go(List[List[A]](), l).reverse

val plF = packF(List("a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e"))
assert(plF == List(List("a", "a", "a", "a"), List("b"), List("c", "c"), List("a", "a"), List("d"), List("e", "e", "e", "e")))
