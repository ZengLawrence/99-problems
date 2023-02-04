// P19 (**) Rotate a list N places to the left.
def rotate[A](n: Int, ls: List[A]): List[A] = 
  val nMod = if ls.isEmpty then 0 else n % ls.length
  if nMod < 0 then rotate(ls.length + nMod, ls)
  else (ls drop nMod)  ++: (ls take nMod)

val r = rotate(3, List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k'))
assert(r == List('d', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'a', 'b', 'c'))

val rr = rotate(-2, List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k'))
assert(rr == List('j', 'k', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i'))

val r12 = rotate(12, List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k'))
assert(r12 == List('b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'a'))

// recursion
def rotateR[A](n: Int, ls: List[A]): List[A] =
  @annotation.tailrec
  def go(n: Int, suffix: List[A], remain: List[A]): List[A] =
    if n == 0 then remain ++: suffix.reverse
    else go(n - 1, remain.head +: suffix, remain.tail)
  
  val nMod = if ls.isEmpty then 0 else n % ls.length
  if (nMod > 0) go(nMod, List[A](), ls)
  else go(ls.length + nMod, List[A](), ls)

val rR = rotateR(3, List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k'))
assert(rR == List('d', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'a', 'b', 'c'))

val rrR = rotateR(-2, List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k'))
assert(rrR == List('j', 'k', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i'))

val rR12 = rotateR(12, List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k'))
assert(rR12 == List('b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'a'))
