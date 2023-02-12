// P32 (**) Determine the greatest common divisor of two positive integer numbers.
// Use Euclidean algorithm

def gcd(a: Int, b: Int): Int = 
  @annotation.tailrec
  def go(a: Int, b: Int): Int = (a, b) match {
    case (a, 0) => a  // don't have to check a for 0 because third condition will flip it
    case (a, b) => go(b, a % b)
  }
  go(a, b)

assert(gcd(36, 63) == 9)
assert(gcd(63, 36) == 9)
assert(gcd(270,192) == 6)

// special cases
assert(gcd(0, 5) == 5)
assert(gcd(5, 0) == 5)
