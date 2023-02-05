// P27 (**) Group the elements of a set into disjoint subsets.
/*
a) In how many ways can a group of 9 people work in 3 disjoint subgroups of 2, 3 and 4 persons? Write a function that generates all the possibilities.
*/
def group3[A](list: List[A]): List[List[List[A]]] =
  def go(n: Int, subsets: List[List[List[A]]], remains: List[A]): List[List[List[A]]] =
    if n == remains.length then (remains +: subsets.head).reverse +: subsets.tail
    else
      remains.combinations(n).toList.flatMap { ss =>
        val newSubsets = if subsets.isEmpty then List(List(ss)) else (ss +: subsets.head) +: subsets.tail
        go(n + 1, newSubsets, remains.filter{a => !ss.contains(a)})
      }
  go(2, Nil, list)

group3(List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida"))
