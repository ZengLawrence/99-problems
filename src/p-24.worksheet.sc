// P24 (*) Lotto: Draw N different random numbers from the set 1..M.

//#region helper functions
// from P-23
import util.Random

def removeAt[A](i: Int, ls: List[A]): (List[A], A) = ls.splitAt(i) match {
  case (Nil, _) if i < 0 => throw new NoSuchElementException
  case (prefix, h :: suffix) => (prefix ++: suffix, h)
  case (_, Nil) => throw new NoSuchElementException
}

def randomSelect[A](n: Int, ls: List[A]): List[A] = 
  // passing around random generator to be more efficient
  @annotation.tailrec
  def go(n: Int, selections: List[A], ls: List[A], rand: Random): List[A] = (n, ls) match {
    case (0, ls) => selections
    case (_, Nil) => selections
    case (n, ls) => {
      val i = rand.nextInt(ls.length)
      val (remains, a) = removeAt(i, ls)
      go(n - 1, a +: selections, remains, rand)
    }
  }

  go(n, List[A](), ls, new Random())
//#endregion

def lotto(n: Int, max: Int): List[Int] =
  assert(n > 0)
  assert(max > 0)
  assert(max > n)
  randomSelect(n, List.range(1, max + 1))

val l = lotto(6, 49)
assert(l.length == 6)