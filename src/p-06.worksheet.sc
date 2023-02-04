def isPalindrome[A](l: List[A]): Boolean =
	l == l.reverse

val b = isPalindrome(List(1, 2, 3, 2, 1))
assert(b)

def isPalindromeR[A](l: List[A]): Boolean = 
	@annotation.tailrec
	def go(reversed: List[A], remaining: List[A], original: List[A]): Boolean = remaining match {
		case Nil => original == reversed
		case h :: t => go(h +: reversed, t, original)
	}
	go(List[A](), l, l)

val br = isPalindromeR(List(1, 2, 3, 2, 1))
assert(br)

def isPalindromeF[A](l: List[A]): Boolean =
	l.foldRight((true,l)){ (a, accum) => 
		val (p, ll) = accum
		(p && (a == ll.head), ll.tail)
	}._1

val bf = isPalindromeF(List(1, 2, 3, 2, 1))

