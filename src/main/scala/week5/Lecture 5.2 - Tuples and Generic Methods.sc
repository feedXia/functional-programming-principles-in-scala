/*
Merge sort
 * If list contains 0 or 1 elements => sorted
 * Otherwise:
 * seperate into 2 sub-lists, each containing ~ half of elements of original list
 * sort 2 sub-lists
 * merge sorted sub-lists into single sorted list
 */
/** @param n
  *   index from which to split the list
  * @return
  *   a pair, where 1st elem is elems of xs up to index n & second elem of pair
  *   are elems of xs except first n elems
  */
extension [T](xs: List[T]) def splitAt(n: Int) = (xs.take(n), xs.drop(n))

val chars = List('a', 'b', 'c', 'd')
chars.splitAt(2)

// Impl outline
def msort(xs: List[Int]): List[Int] =
  val mid = xs.length / 2
  if mid == 0 then xs // length of 1 or 0
  else
    /** @param left
      *   list of ints assumed to be sorted
      * @param right
      *   list of ints assumed to be sorted
      * @return
      */
    def merge(left: List[Int], right: List[Int]): List[Int] =
      (left, right) match
        case (Nil, right) => right
        case (left, Nil)  => left
        case (lhead :: ltail, rhead :: rtail) => // both lists non-empty
          if lhead < rhead then
            lhead :: merge(
              ltail,
              right
            ) // list starts with lhead as smaller followerd by merge of rest of left & right
          else rhead :: merge(left, rtail)
    val (l, r) = xs.splitAt(
      mid
    ) // returns 2 sublists in a pair (see below): elements up to given index, elements from that index
    merge(
      msort(l),
      msort(r)
    ) // recursively sort l and r using merge sort & merge result of two recursive sorts

/* Generalise merge sort
    Does not work because the comparison < is not defined for arbitrary types T
    idea: parameterise helper function merge with necessary comparison function
 */
/** Generalised polymorphic merge sort function
  * @param xs
  *   list to sort
  * @param lt
  *   comparison function less than that takes 2 elements of type T and returns
  *   a boolean
  * @tparam T
  * @return
  *   sorted list of T
  */
def msort1[T](xs: List[T])(lt: (T, T) => Boolean): List[T] =
  val mid = xs.length / 2
  if mid == 0 then xs
  else
    def merge(left: List[T], right: List[T]): List[T] = (left, right) match
      case (Nil, right) => right
      case (left, Nil)  => left
      case (lhead :: ltail, rhead :: rtail) =>
        if lt(lhead, rhead) then lhead :: merge(ltail, right)
        else rhead :: merge(left, rtail)
    val (l, r) = xs.splitAt(mid)
    merge(msort1(l)(lt), msort1(r)(lt))

val ints = List(-5, 6, 3, 2, 7)
val fruits = List("apple", "orange", "pear", "pineapple")

msort1(ints)((x: Int, y: Int) => x < y)
msort1(ints)((x, y) => x < y) // since parameter types can be inferred from the call msort(xs). Can we infer parameters from their types? I.e. pick out the correct comparison function based on the type of the xs param? Yes! Implicits, to come in future weeks.
msort1(fruits)((x: String, y: String) => x.compareTo(y) < 0)

/* Scala: pair
 * consisting of x & y is written (x, y)
    Can be of different types
 */
val pair = ("answer", 42)
// can also be used as pattern
val (label, value) = pair // preferred
// analogous for tuples with > 2 elements
val label1 =
  pair._1 // equivalent, sometimes useful with long tuple & want to select single element
val value1 = pair._2

val nums = List(1, 2, 3, 4, 5, 6, 7, 8)
nums.take(3)
nums.drop(3)

/*
So far in Scala, every type was an instance of some class - same for tuples
 */

case class Tuple2[+T1, +T2](
    _1: T1,
    _2: T2
): // 2 selectors _1 & _2, tuples are covariant in both argument types, case class so selector values also available as fields
  override def toString = "(" + _1 + "," + _2 + ")" // Looks like a pair
