def reverse[A](l: List[A]): List[A] =
	l.reverse

reverse(List(1, 1, 2, 3, 5, 8))

def reverseR[A](l: List[A]): List[A] = 
	@annotation.tailrec
	def go(result: List[A], l: List[A]): List[A] = l match {
		case Nil => result
		case h :: t => go(h :: result, t)
	}
	go(Nil, l)

reverseR(List(1, 1, 2, 3, 5, 8))

def reverseF[A](l: List[A]): List[A] = 
	l.foldLeft(List[A]()){ (reversedList, a) => a +: reversedList }

reverseF(List(1, 1, 2, 3, 5, 8))
