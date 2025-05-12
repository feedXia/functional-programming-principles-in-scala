import org.graalvm.compiler.hotspot.nodes.aot.EncodedSymbolNode.encode

/** MAPPING
  */

/*
  Common op: transform each elem of list & return list of results

e.g. multiply elements by a factor
 */

def scaleList(xs: List[Double], factor: Double): List[Double] = xs match
  case Nil          => Nil
  case head :: tail => head * factor :: scaleList(tail, factor)

/*
  Generalise into method map of List class
  Very useful for all cases transforming lems of list given some transform function
 */
extension [T](xs: List[T])
  def map[U](f: T => U): List[U] = xs match
    case Nil          => Nil
    case head :: tail => f(head) :: tail.map(f)

// Re-writing scaleList
def scaleListWithMap(xs: List[Double], factor: Double): List[Double] =
  xs.map(factor * _)

/*
  Function to square each element of a list & return result
 */

def squareList(xs: List[Int]): List[Int] = xs match
  case Nil          => Nil
  case head :: tail => head * head :: squareList(tail)

def squareListWithMap(xs: List[Int]): List[Int] = xs.map(x => x * x)

/** FILTERING Selection of all elements satisfying a given condition
  */

def posElems(xs: List[Int]): List[Int] = xs match
  case Nil => xs
  case head :: tail =>
    if head > 0 then head :: posElems(tail) else posElems(tail)

/*
  Generalise into method filter of List class
 */

extension [T](xs: List[T])
  def filter(p: T => Boolean): List[T] = xs match
    case Nil => xs
    case head :: tail =>
      if p(head) then head :: tail.filter(p) else tail.filter(p)

def posElemsWithFilter(xs: List[Int]): List[Int] = xs.filter(_ > 0)

// Filter variations
val xs = List(1, 2, 3)
xs.filterNot(_ < 0) // Extracts elems that do no satisfy predicate
xs.partition(
  _ > 0
) // Returns tuple of (xs.filter, xs.filterNot) in single traversal
xs.takeWhile(
  _ < 3
) // Extracts elems from beginning as long as predicate satisfied. First elem does not satisfy predicate terminates output.
xs.dropWhile(
  _ < 2
) // Drops elems from beginning of list while predicate satisfied until first elem that satisfies predicate
xs.span(
  _ < 2
) // Returns tuple of (xs.takeWhile, xs.dropWhile) in single traversal

val nums = List(1, 2, 3, 4, 5, 6)
nums.partition(_ % 2 != 0)
nums.span(_ % 2 != 0)

/*
Exercise 1
Function that packs consecutive duplicates of list elems into sublists
 */

def pack[T](xs: List[T]): List[List[T]] = xs match
  case Nil => Nil
  case head :: _ =>
    val (dups, rest) = xs.span(_ == head)
    dups :: pack(rest)

def packMartin[T](xs: List[T]): List[List[T]] = xs match
  case Nil => Nil
  case h :: t =>
    val (more, rest) = t.span(_ == h)
    (h :: more) :: pack(rest)

val consecutiveStrs = List("a", "a", "a", "b", "c", "c", "a")
pack(consecutiveStrs)
packMartin(consecutiveStrs)

/*
Exercise 2
Use pack, function encode that produces run-length encoding of a list
Encode n consecutive duplicates of an elem x as a pair (x, n)
 */

def encode[T](xs: List[T]): List[(T, Int)] =
  packMartin(xs).map(consecList => (consecList.head, consecList.length))
encode(consecutiveStrs)
