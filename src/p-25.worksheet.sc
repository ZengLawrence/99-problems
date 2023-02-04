// P25 (*) Generate a random permutation of the elements of a list.

//#region help function from p-23
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

def randomPermute[A](ls: List[A]): List[A] = 
  randomSelect(ls.length, ls)

val rp = randomPermute(List('a', 'b', 'c', 'd', 'e', 'f'))
assert(rp.length == 6)

// in place permutation
import scala.reflect.ClassTag

// Note: need to using implicit ClassTag for Array due to type erasure
def randomPermuteInPlace[A](ls: List[A])(implicit tag: ClassTag[A]): List[A] = 
  val rand = new Random()
  val arr = ls.toArray
  for (i <- arr.length - 1 to 1 by -1) {
    val j = rand.nextInt(i + 1)
    val t = arr(i)
    arr(i) = arr(j)
    arr(j) = t
  }
  arr.toList

val rpi = randomPermuteInPlace(List('a', 'b', 'c', 'd', 'e', 'f'))
assert(rpi.length == 6)
