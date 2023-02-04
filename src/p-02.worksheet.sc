def penultimate[A](l: List[A]): A =
	l match {
		case p :: _ :: Nil => p 
		case _ :: t => penultimate(t)
		case _ => throw RuntimeException("too few elements")
	}

penultimate(List(1, 1, 2, 3, 5, 8))

def penultimateS[A](l: List[A]): A =
	l.sliding(2).toList.last.head

penultimateS(List(1, 1, 2, 3, 5, 8))

// build-in method 'init'
def penultimateB[A](l: List[A]): A =
	l.init.last

penultimateB(List(1, 1, 2, 3, 5, 8))
