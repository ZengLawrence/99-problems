// P41 (**) A list of Goldbach compositions.

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
    (2 to n / 2) find {i => i.isPrime && (n -i).isPrime} match {
      case Some(i) => (i, n - i)
      case None => throw new IllegalArgumentException("Glodbach not found for " + n)
    }

  def isEven: Boolean = (n % 2) == 0

def goldbachList(range: Range): Map[Int, (Int, Int)] =
    range.filter{i => i > 2 && i.isEven}.groupMapReduce {identity} {_.goldbach} {(_, pair) => pair}

assert(goldbachList(9 to 20) == Map(10 -> (3, 7),
                                    12 -> (5, 7),
                                    14 -> (3, 11),
                                    16 -> (3, 13),
                                    18 -> (5, 13),
                                    20 -> (3, 17))
)

assert(goldbachList(1 to 4) == Map(4 -> (2, 2)))

// limit list with a minimum prime
def goldbachListLimited(range: Range, minPrime: Int): Map[Int, (Int, Int)] =
  goldbachList {Range.inclusive(minPrime, range.`end`)} filter {(_, v) => (v._1 > minPrime && v._2 > minPrime)}

assert(goldbachListLimited(1 to 2000, 50) == Map(992 -> (73, 919),
                                                1382 -> (61, 1321),
                                                1856 -> (67, 1789),
                                                1928 -> (61, 1867))
)
