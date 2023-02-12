// P31 (**) Determine whether a given integer number is prime.
extension (n: Int)
  def isPrime: Boolean = n match {
    case n if n > 1 => {
      val (_, potentialDivisors) = (2 to Math.sqrt(n).toInt by 2) span {n % _ > 0}
      potentialDivisors.length == 0
    }
    case _ => false
  }

assert(2.isPrime == true)
assert(3.isPrime == true)
assert(4.isPrime == false)
assert(5.isPrime == true)
assert(6.isPrime == false)
assert(7.isPrime == true)

// special cases
assert(1.isPrime == false)
assert(0.isPrime == false)
assert(-1.isPrime == false)
