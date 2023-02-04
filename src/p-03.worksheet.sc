def nth[A](n: Int, l: List[A]): A = 
	if n >= 0 then l(n)
		else throw new NoSuchElementException

val n2 = nth(2, List(1, 1, 2, 3, 5, 8))
assert(n2 == 2)

def nthR[A](n: Int, l: List[A]): A =
	@annotation.tailrec
	def go(n: Int, l: List[A]): A = (n, l) match {
			case (0, h :: _) => h 
			case (n, _ :: t) => go(n-1, t)
			case _ => throw new NoSuchElementException
		} 
	go(n, l)

val n2r = nthR(2, List(1, 1, 2, 3, 5, 8))
assert(n2r == 2)
