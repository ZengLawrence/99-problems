// P33 (*) Determine whether two positive integer numbers are coprime
// Two numbers are coprime if their greatest common divisor equals 1.

def gcd(a: Int, b: Int): Int = 
  @annotation.tailrec
  def go(a: Int, b: Int): Int = (a, b) match {
    case (a, 0) => a  // don't have to check a for 0 because third condition will flip it
    case (a, b) => go(b, a % b)
  }
  go(a, b)

extension (a: Int) 
  def isCoprimeTo(b: Int): Boolean = gcd(a, b) == 1

assert(35.isCoprimeTo(64) == true)