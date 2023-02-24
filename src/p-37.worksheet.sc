// P37 (**) Calculate Euler’s totient function ϕ(m) (improved).

extension (n: Int)
  def isPrime: Boolean = n match {
    case 2 => true
    case n if n > 1 => {
      val potentialDivisors = (2 +: (3 to Math.sqrt(n).toInt by 2)) dropWhile {n % _ > 0}
      potentialDivisors.length == 0
    }
    case _ => false
  }

  def primeFactors: List[Int] =
    @annotation.tailrec
    def go(primeToTest: LazyList[Int], factors: List[Int], number: Int): List[Int] = (primeToTest, number) match {
      case (_, 1) => factors
      case (LazyList(), n) => n +: factors
      case (p #:: _, n) if n % p == 0 => go(primeToTest, p +: factors, n / p)
      case (p #:: t, n) => go(t, factors, n)
    }
    val primes = 2 #:: LazyList.from(3, 2) filter {_.isPrime}
    go(primes, List[Int](), n).reverse

  // Alternative as suggested by the website is to count directly. However, that design packs too
  // much in one function. This is a better approach.
  def primeFactorMultiplicity: List[(Int, Int)] =
    (for 
      (k, v) <- n.primeFactors groupBy {i => i}
    yield (k, v.length)).toList

  def totient: Int = 
    n.primeFactorMultiplicity map {(p, m) => (p - 1)*(Math.pow(p, m - 1)).toInt} reduce {_ * _}

end extension

assert(10.totient == 4)