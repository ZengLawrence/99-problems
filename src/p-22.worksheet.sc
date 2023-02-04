// P22 (*) Create a list containing all integers within a given range.
def range(start: Int, endInclusive: Int): List[Int] =
  Range.inclusive(start, endInclusive).toList

val r = range(4, 9)
assert(r == List(4, 5, 6, 7, 8, 9))

// recursion
def rangeR(start: Int, endInclusive: Int): List[Int] =
  @annotation.tailrec
  def go(end: Int, ls: List[Int]): List[Int] =
    if end < start then ls
    else go(end - 1, end +: ls)

  go(endInclusive, List[Int]())

val rr = rangeR(4, 9)
assert(rr == List(4, 5, 6, 7, 8, 9))

// functional using unfold
def rangeF(start: Int, endInclusive: Int): List[Int] =
  List.unfold(start) { n =>
    if n > endInclusive then None
    else Some(n, n + 1)
  }

val rf = rangeF(4, 9)
assert(rf == List(4, 5, 6, 7, 8, 9))
