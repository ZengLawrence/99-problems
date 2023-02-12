// P31 (**) Determine whether a given integer number is prime.

extension (n: Int)
  def isPrime: Boolean = n match {
    case 2 => true
    case _ => {
      val (_, potentialDivisors) = (2 until n by 2) span {n % _ > 0}
      potentialDivisors.length == 0
    }
  }

assert(2.isPrime == true)
assert(3.isPrime == true)
assert(4.isPrime == false)
assert(5.isPrime == true)
assert(6.isPrime == false)
assert(7.isPrime == true)