// P27 (**) Group the elements of a set into disjoint subsets.
/*
a) In how many ways can a group of 9 people work in 3 disjoint subgroups of 2, 3 and 4 persons? Write a function that generates all the possibilities.
*/
def group3[A](list: List[A]): List[List[A]] =
  def go(n: Int, subsets: List[List[A]], remains: List[A]): List[List[A]] =
    if n == remains.length then (remains +: subsets).reverse
    else
      remains.combinations(n).toList.flatMap { ss =>
        go(n + 1, ss +: subsets, remains.filter{a => !ss.contains(a)})
      }
  go(2, List[List[A]](), list)

group3(List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida"))
