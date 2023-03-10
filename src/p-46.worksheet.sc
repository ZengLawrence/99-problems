// P46 (**) Truth tables for logical expressions.

def table2(f: (a: Boolean, b: Boolean) => Boolean): Map[(Boolean, Boolean), Boolean] =
  (for
    a <- List(true, false) 
    b <- List(true, false)
  yield {(a, b) -> f(a, b)}).groupMapReduce{_._1}{_._2}{(_, v) => v}

def not(a: Boolean): Boolean = a match {
  case true => false
  case false => true
}

def and(a: Boolean, b: Boolean): Boolean = (a, b) match {
  case (true, true) => true
  case _ => false
}

assert(table2(and) == Map((true,true) -> true, 
                          (true,false) -> false, 
                          (false,true) -> false, 
                          (false,false) -> false)
)

def or(a: Boolean, b: Boolean): Boolean = (a, b) match {
  case (false, false) => false
  case _ => true
}

assert(table2(or) == Map((true,true) -> true, 
                          (true,false) -> true, 
                          (false,true) -> true, 
                          (false,false) -> false)
)

// test example
val t = ((a: Boolean, b: Boolean) => and(a, or(a, b)))
assert(table2(t) == Map((true,true) -> true, 
                        (true,false) -> true, 
                        (false,true) -> false, 
                        (false,false) -> false)
)

def nand(a: Boolean, b: Boolean): Boolean = not(and(a: Boolean, b: Boolean))

assert(table2(nand) == Map((true,true) -> false, 
                          (true,false) -> true, 
                          (false,true) -> true, 
                          (false,false) -> true)
)

def nor(a: Boolean, b: Boolean): Boolean = not(or(a: Boolean, b: Boolean))

assert(table2(nor) == Map((true,true) -> false, 
                          (true,false) -> false, 
                          (false,true) -> false, 
                          (false,false) -> true)
)

def xor(a: Boolean, b: Boolean): Boolean = or(and(a, not(b)), and(not(a), b))

assert(table2(xor) == Map((true,true) -> false, 
                          (true,false) -> true, 
                          (false,true) -> true, 
                          (false,false) -> false)
)

def impl(a: Boolean, b: Boolean): Boolean = or(not(a), b)

assert(table2(impl) == Map((true,true) -> true, 
                          (true,false) -> false, 
                          (false,true) -> true, 
                          (false,false) -> true)
)

def equ(a: Boolean, b: Boolean): Boolean = and(impl(a, b), impl(not(a), not(b)))

assert(table2(equ) == Map((true,true) -> true, 
                          (true,false) -> false, 
                          (false,true) -> false, 
                          (false,false) -> true)
)