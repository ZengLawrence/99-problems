import scala.reflect.ClassTag
import util.Random

// in place permutation
def randomPermuteInPlace[A : ClassTag](ls: List[A]): List[A] = 
  val rand = new Random()
  val arr = ls.toArray
  Range(1, arr.length).reverse.foreach { i => 
    val j = rand.nextInt(i + 1)
    val t = arr(i)
    arr(i) = arr(j)
    arr(j) = t
  }
  arr.toList

val rpi = randomPermuteInPlace(List('a', 'b', 'c', 'd', 'e', 'f'))
assert(rpi.length == 6)