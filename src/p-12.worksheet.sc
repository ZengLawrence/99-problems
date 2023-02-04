// Given a run-length code list generated as specified in problem P10, construct its uncompressed version.

def decode[A](l: List[(Int, A)]): List[A] =
  l flatMap {(n, a) => List.fill(n)(a)}

val d = decode(List((4,"a"), (1,"b"), (2,"c"), (2,"a"), (1,"d"), (4,"e")))
assert(d == List("a", "a", "a", "a", "b", "c", "c", "a", "a", "d", "e", "e", "e", "e"))