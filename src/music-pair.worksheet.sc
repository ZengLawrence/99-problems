// find music pairs of multiple of 60's
def musicPair(songs: List[Int]): Long = songs match {
  case Nil => 0
  case h :: t => t.filter(n => (n + h) % 60 == 0).length + musicPair(t)
}

assert(musicPair(List(20, 40, 60)) == 1)
assert(musicPair(List(20, 40, 60, 60)) == 2)
assert(musicPair(List(30, 30, 30)) == 3)
assert(musicPair(List(120, 60, 30)) == 1)

def musicPair2(songs: List[Int]): Long = 
  @annotation.tailrec
  def go(count: Long, remains: List[Int]): Long = remains match {
    case Nil => count
    case h :: t => go(count + t.filter(n => (n + h) % 60 == 0).length, t)
  }
  go(0, songs)

assert(musicPair2(List(20, 40, 60)) == 1)
assert(musicPair2(List(20, 40, 60, 60)) == 2)
assert(musicPair2(List(30, 30, 30)) == 3)
assert(musicPair2(List(120, 60, 30)) == 1)

// frequency map
def musicPairFM(songs: List[Int]): Long = 
  val init = (0 to 59).map(_ -> 0).toMap
  val freqMap = init ++ songs.map(len => len % 60).groupMapReduce(identity)(_ => 1)(_ + _)
  val pair1To29 = (1 to 29).map(m => freqMap(m) * freqMap(60 - m)).sum
  val pair30 = (freqMap(30) * (freqMap(30) - 1)) / 2
  val pair0 = (freqMap(0) * (freqMap(0) - 1)) / 2
  pair0 + pair1To29 + pair30

val r = musicPairFM(List(30, 30, 30))

assert(musicPairFM(List(20, 40, 60)) == 1)
assert(musicPairFM(List(20, 40, 60, 60)) == 2)
assert(musicPairFM(List(30, 30, 30)) == 3)
assert(musicPairFM(List(120, 60, 30)) == 1)
