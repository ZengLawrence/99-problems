// P39 (*) A list of prime numbers.
extension (n: Int)
  def isPrime: Boolean = n match {
    case 2 => true
    case n if n > 1 => {
      val potentialDivisors = (2 +: (3 to Math.sqrt(n).toInt by 2)) dropWhile {n % _ > 0}
      potentialDivisors.length == 0
    }
    case _ => false
  }

def listPrimesInRange(range: Range): List[Int] =
  range.filter{_.isPrime}.toList

assert(listPrimesInRange(7 to 31) == List(7, 11, 13, 17, 19, 23, 29, 31))