// P40 (**) Goldbach’s conjecture.

extension (n: Int)
  def isPrime: Boolean = n match {
    case 2 => true
    case n if n > 1 => {
      val potentialDivisors = (2 +: (3 to Math.sqrt(n).toInt by 2)) dropWhile {n % _ > 0}
      potentialDivisors.length == 0
    }
    case _ => false
  }

  def goldbach: (Int, Int) = 
    def notPrimePair(i: Int): Boolean = !i.isPrime || !(n -i).isPrime
    ((2 to n / 2) dropWhile {notPrimePair} map {i => (i, n - i)}).head

assert(28.goldbach == (5,23))