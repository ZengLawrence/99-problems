// P32 (**) Determine the greatest common divisor of two positive integer numbers.
// Use Euclidean algorithm

def gcd(a: Int, b: Int): Int = 
  @annotation.tailrec
  def go(a: Int, b: Int): Int = (a, b) match {
    case (0, b) => b
    case (a, 0) => a
    case (a, b) => go(b, a % b)
  }
  go(a, b)

assert(gcd(36, 63) == 9)
assert(gcd(63, 36) == 9)
assert(gcd(270,192) == 6)
