def flatten[Any](l: List[Any]): List[Any] = l flatMap {
  a => a match
    case la: List[Any] => flatten(la)
    case e => List(e)
}

flatten(List(List(1, 1), 2, List(3, List(5, 8))))