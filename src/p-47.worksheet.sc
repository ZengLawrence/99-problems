//P47 (*) Truth tables for logical expressions (2).

def table2(f: (a: Boolean, b: Boolean) => Boolean): Map[(Boolean, Boolean), Boolean] =
  (for
    a <- List(true, false) 
    b <- List(true, false)
  yield {(a, b) -> f(a, b)}).groupMapReduce{_._1}{_._2}{(_, v) => v}

def not(a: Boolean): Boolean = a match {
  case true => false
  case false => true
}

extension (a: Boolean)
  def and(b: Boolean): Boolean = (a, b) match {
    case (true, true) => true
    case _ => false
  }

  def or(b: Boolean): Boolean = (a, b) match {
    case (false, false) => false
    case _ => true
  }

  def nand(b: Boolean): Boolean = not(a and b)
  def nor(b: Boolean): Boolean = not(a or b)
  def xor(b: Boolean): Boolean = (a and not(b)) or (not(a) and b)

// and
assert(table2((a, b) => a and b) == Map((true,true) -> true, 
                                        (true,false) -> false, 
                                        (false,true) -> false, 
                                        (false,false) -> false)
)

// or
assert(table2((a, b) => a or b) == Map((true,true) -> true, 
                                        (true,false) -> true, 
                                        (false,true) -> true, 
                                        (false,false) -> false)
)

// test example
val t = ((a: Boolean, b: Boolean) => a and (a or b))
assert(table2(t) == Map((true,true) -> true, 
                        (true,false) -> true, 
                        (false,true) -> false, 
                        (false,false) -> false)
)

// nand
assert(table2((a, b) => a nand b) == Map((true,true) -> false, 
                                        (true,false) -> true, 
                                        (false,true) -> true, 
                                        (false,false) -> true)
)

// nor
assert(table2((a, b) => a nor b) == Map((true,true) -> false, 
                                        (true,false) -> false, 
                                        (false,true) -> false, 
                                        (false,false) -> true)
)

// xor
assert(table2((a, b) => a xor b) == Map((true,true) -> false, 
                                        (true,false) -> true, 
                                        (false,true) -> true, 
                                        (false,false) -> false)
)