// P28 (**) Sorting a list of lists according to length of sublists.

/**
    * a) We suppose that a list contains elements that are lists themselves.  
    * The objective is to sort the elements of the list according to their 
    * length.  E.g. short lists first, longer lists later, or vice versa.
    *
    * @param list
    * @return
    */
def lsort[A](list: List[List[A]]): List[List[A]] =
  list map {sl => (sl.length, sl)}  sortWith {(a, b) => a._1.compareTo(b._1) < 0} map {_._2}

val ls = lsort(List(List("a", "b", "c"), List("d", "e"), List("f", "g", "h"), List("d", "e"), List("i", "j", "k", "l"), List("m", "n"), List("o")))
assert(ls == List(List("o"), List("d", "e"), List("d", "e"), List("m", "n"), List("a", "b", "c"), List("f", "g", "h"), List("i", "j", "k", "l")))

/**
    * Again, we suppose that a list contains elements that are lists themselves.  
    * But this time the objective is to sort the elements according to their 
    * length frequency; i.e. in the default, sorting is done ascendingly, 
    * lists with rare lengths are placed, others with a more frequent length 
    * come later.
    *
    * @param list
    * @return
    */
def lsortFreq[A](list: List[List[A]]): List[List[A]] =
  lsort(list.groupBy(_.length).values.toList) flatMap {a => a}

val lfs = lsortFreq(List(List("a", "b", "c"), List("d", "e"), List("f", "g", "h"), List("d", "e"), List("i", "j", "k", "l"), List("m", "n"), List("o")))
assert(lfs == List(List("o"), List("i", "j", "k", "l"), List("a", "b", "c"), List("f", "g", "h"), List("d", "e"), List("d", "e"), List("m", "n")))