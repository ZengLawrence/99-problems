// P46 (**) Truth tables for logical expressions.

def table2(f: (a: Boolean, b: Boolean) => Boolean): Map[(Boolean, Boolean), Boolean] =
  {List(true, true, false, false) zip List(true, false, true, false)}.groupMapReduce{identity}{ab => f(ab._1, ab._2)}{(_, v) => v}

def and(a: Boolean, b: Boolean): Boolean = a && b

assert(table2(and) == Map((true,true) -> true, 
                          (true,false) -> false, 
                          (false,false) -> false, 
                          (false,true) -> false)
)

def or(a: Boolean, b: Boolean): Boolean = a || b

assert(table2(or) == Map((true,true) -> true, 
                          (true,false) -> true, 
                          (false,false) -> false, 
                          (false,true) -> true)
)

// test example
val t = ((a: Boolean, b: Boolean) => and(a, or(a, b)))
assert(table2(t) == Map((true,true) -> true, 
                        (true,false) -> true, 
                        (false,false) -> false, 
                        (false,true) -> false)
)

def nand(a: Boolean, b: Boolean): Boolean = !and(a: Boolean, b: Boolean)

assert(table2(nand) == Map((true,true) -> false, 
                          (true,false) -> true, 
                          (false,true) -> true, 
                          (false,false) -> true)
)

def nor(a: Boolean, b: Boolean): Boolean = !or(a: Boolean, b: Boolean)

table2(nor)

assert(table2(nor) == Map((true,true) -> false, 
                          (true,false) -> false, 
                          (false,true) -> false, 
                          (false,false) -> true)
)
