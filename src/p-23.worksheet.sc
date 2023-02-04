// P23 (**) Extract a given number of randomly selected elements from a list.
import scala.util.Random

// from P-20
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

val r = randomSelect(3, List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'))
assert(r.length == 3)

val r0 = randomSelect(3, List())
assert(r0 == Nil)

val r2 = randomSelect(3, List('a', 'b'))
assert(r2.length == 2)

// with currying
def randomSelectC[A](n: Int, ls: List[A]): List[A] = 
  // passing around random generator to be more efficient
  @annotation.tailrec
  def go(n: Int, selections: List[A], ls: List[A])(next: Int => Int): List[A] = (n, ls) match {
    case (0, ls) => selections
    case (_, Nil) => selections
    case (n, ls) => {
      val i = next(ls.length)
      val (remains, a) = removeAt(i, ls)
      go(n - 1, a +: selections, remains)(next)
    }
  }

  val rand = new Random()
  val next = (bound: Int) => rand.nextInt(bound)
  go(n, List[A](), ls)(next)

val rc = randomSelectC(3, List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'))
assert(rc.length == 3)
