// P27 (**) Group the elements of a set into disjoint subsets.
/*
a) In how many ways can a group of 9 people work in 3 disjoint subgroups of 2, 3 and 4 persons? Write a function that generates all the possibilities.
*/
def combinations[A](n: Int, list: List[A]): List[List[A]] = 
  list.combinations(n).toList

def group3[A](list: List[A]): List[List[List[A]]] =
  for {
    a <- combinations(2, list)
    noA = list.filterNot(a.contains(_))
    b <- combinations(3, noA)
  } yield List(a, b, noA.filterNot(b.contains(_)))

group3(List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida"))

/*
b) Generalize the above predicate in a way that we can specify a list of group sizes and the predicate will return a list of groups.
*/
def group[A](sizes: List[Int], list: List[A]): List[List[List[A]]] = sizes match {
  case Nil => List(Nil)
  case n :: t => combinations(n, list) flatMap { s =>
    group(t, (list filterNot {s.contains(_)})) map {s +: _}
  }
}

group(List(2, 2, 5), List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida"))
assert{
  group3(List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida")) == group(List(2, 3, 4), List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida"))
}