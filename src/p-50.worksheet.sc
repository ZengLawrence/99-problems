// P50 (***) Huffman code.

/**
    * implementation based on book Introduction to Data Compression, 4th Edition, 
    * chapter 3 Huffman Code. Recursion method.
    */
def huffman(symFreq: List[(String, Int)]): List[(String, Int)] = 
  symFreq.sortWith((a, b) => a._2.compareTo(b._2) < 0) match {
    case Nil => Nil
    case h :: Nil => List((h._1, 0))
    case l1 :: l2 :: remains => {
      val newSymbolFreq = (l1._1 ++ l2._1, l1._2 + l2._2)
      val codes = huffman(newSymbolFreq +: remains)
      val newSymbolCode = codes.find(_._1 == newSymbolFreq._1).get._2
      (l1._1, newSymbolCode * 10 + 0) +: (l2._1, newSymbolCode * 10 + 1) +: codes.filterNot(_._1 == newSymbolFreq._1)
    }
  }

val codes = huffman(List(("a", 45), ("b", 13), ("c", 12), ("d", 16), ("e", 9), ("f", 5)))
assert(codes.sorted == List(("a",0), ("b",101), ("c",100), ("d",111), ("e",1101), ("f",1100)))