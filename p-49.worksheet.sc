// P49 (**) Gray code.

// An n-bit Gray code is a sequence of n-bit strings constructed according to certain rules.

/**
    * The code is called reflected because it can be generated in the following 
    * manner. Take the Gray code 0, 1. Write it forwards, then backwards: 0, 1, 
    * 1, 0. Then prepend 0s to the first half and 1s to the second half: 00, 01, 
    * 11, 10. Continuing, write 00, 01, 11, 10, 10, 11, 01, 00 to obtain: 000, 
    * 001, 011, 010, 110, 111, 101, 100, ... (OEIS A014550). Each iteration 
    * therefore doubles the number of codes.
    *
    * @param n
    * @return
    */
def gray(n: Int): List[String] =
  @annotation.tailrec
  def go(codes: List[String], n: Int): List[String] = (codes, n) match {
    case (cs, 0) => cs
    case (Nil, n) => go(List("0", "1"), n - 1)
    case (cs, n) => {
      val prefix = for c <- cs yield { "0" + c }
      val suffix = for c <- cs.reverse yield { "1" + c }
      go(prefix ++: suffix, n - 1)
    }
  }
  go(List(), n)

assert(gray(1) == List("0", "1"))
assert(gray(2) == List("00", "01", "11", "10"))
assert(gray(3) == List("000", "001", "011", "010", "110", "111", "101", "100"))