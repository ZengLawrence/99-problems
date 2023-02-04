def length[A](l: List[A]): Int =
	l.length

val len = length(List(1, 1, 2, 3, 5, 8))
assert(len == 6)

def lengthR[A](l: List[A]): Int =
	@annotation.tailrec
	def go(n: Int, l: List[A]): Int = l match {
		case Nil => n 
		case _ :: t => go(n+1, t)
	}
	go(0, l)

val lenR = lengthR(List(1, 1, 2, 3, 5, 8))
assert(lenR == 6)

// fold solution
def lengthF[A](l: List[A]): Int = 
	l.foldLeft(0){ (len, _) => len + 1 }

val lenF = lengthF(List(1, 1, 2, 3, 5, 8))
assert(lenF == 6)
