case class P09(name: String)

object P09 {
  def pack[A](l: List[A]): List[List[A]] =
    @annotation.tailrec
    def go(pl: List[List[A]], l: List[A]): List[List[A]] = l match
      case Nil => pl
      case h :: _ => {
        val (nl, t) = l.span(_ == h)
        go(nl +: pl, t)
      } 
    go(List[List[A]](), l).reverse

  def packR[A](l: List[A]): List[List[A]] =
    @annotation.tailrec
    def go(pl: List[List[A]], l: List[A]): List[List[A]] = (pl, l) match
      case (_, Nil) => pl // terminating condition
      case (Nil, (h :: t)) => go(List(List(h)), t) // for first list
      case ((ll :: lt), (h :: t)) if (ll.head == h) => go((h +: ll) +: lt, t) // add to pack list
      case (_, (h :: t)) => go(List(h) +: pl, t) // start a new pack list
    go(List[List[A]](), l).reverse

}