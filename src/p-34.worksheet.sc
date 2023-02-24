// P34 (**) Calculate Euler’s totient function ϕ(m).
/*
 * Euler’s so-called totient function ϕ(m) is defined as the number of positive integers r(1<=r<=m) that are coprime to m.
 */

def gcd(a: Int, b: Int): Int = 
  @annotation.tailrec
  def go(a: Int, b: Int): Int = (a, b) match {
    case (a, 0) => a  // don't have to check a for 0 because third condition will flip it
    case (a, b) => go(b, a % b)
  }
  go(a, b)

extension (a: Int) 
  def isCoprimeTo(b: Int): Boolean = gcd(a, b) == 1
  def totient: Int = 
    ((1 to a) filter {_.isCoprimeTo(a)}).length

assert(10.totient == 4)