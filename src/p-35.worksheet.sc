// P35 (**) Determine the prime factors of a given positive integer. 

// Construct a flat list containing the prime factors in ascending order.

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

assert(315.primeFactors == List(3, 3, 5, 7))