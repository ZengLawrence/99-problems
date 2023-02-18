// P27 (**) Group the elements of a set into disjoint subsets.
/*
a) In how many ways can a group of 9 people work in 3 disjoint subgroups of 2, 3 and 4 persons? Write a function that generates all the possibilities.
*/
def combinations[A](n: Int, list: List[A]): List[List[A]] = 
  list.combinations(n).toList

extension [A] (a: List[A])
  def --(b: List[A]): List[A] = a filterNot {b.contains(_)}

val l1 = List(1, 2, 3)
val r = l1 -- List(2)
assert((l1 -- List(2)) == List(1, 3))

def group3[A](list: List[A]): List[List[List[A]]] =
  for
    a <- combinations(2, list)
    noA = list -- a
    b <- combinations(3, noA)
  yield List(a, b, noA -- b)

group3(List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida"))

/*
b) Generalize the above predicate in a way that we can specify a list of group sizes and the predicate will return a list of groups.
*/
def group[A](sizes: List[Int], list: List[A]): List[List[List[A]]] = sizes match {
  case Nil => List(Nil)
  case n :: t => for
    sg <- combinations(n, list)
    rest <- group(t, (list -- sg))
  yield { sg +: rest }
}

group(List(2, 2, 5), List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida"))
assert{
  group3(List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida")) == group(List(2, 3, 4), List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida"))
}