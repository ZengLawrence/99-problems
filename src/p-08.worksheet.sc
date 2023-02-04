def compress[A](l: List[A]): List[A] =
  l.foldRight(List[A]())( (a, cl) =>
      if (!cl.isEmpty && cl.head == a) then cl 
      else a +: cl
    )

compress(List("a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e"))

def compressR[A](l: List[A]): List[A] = l match {
  case Nil => Nil
  case h :: t => {
     h +: compressR(t.dropWhile(_ == h))
  }
}

compressR(List("a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e"))

def compressTR[A](l: List[A]): List[A] = 
  @annotation.tailrec
  def go(cl: List[A], l: List[A]): List[A] = l match {
    case Nil => cl
    case h :: t => go(h +: cl, t.dropWhile(_ == h))
  }
  go(List[A](), l).reverse

compressTR(List("a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e"))
