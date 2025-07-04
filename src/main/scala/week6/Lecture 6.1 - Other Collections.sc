val nums = Vector(1, 2, 3, -88)
val xs = Vector(5, 6, -90)
val people = Vector("Bob", "James", "Peter")

/*
Since vectors symmetric, left & right adds are equal in efficiency
: always points towards sequence
 */
// Prepend
0 +: nums

// Append
nums :+ -89

// Other kinds of Seqs in Scala collections lib
/*
Arrays & Strings
 * Support the same ops as Seqs & can be implicitly converted to Seqs
 * Cannot be subclasses of Seq as comes from Java
 */

val arr: Array[Int] = Array(1, 2, 3)
arr.map(2 * _)

val str: String = "Hello world!"
str.filter(_.isUpper)

/*
Range
 * Seq of evenly spaced integers
 * 3 operators:
  1. to (inclusive)
  2. until (exclusive)
  3. by (to determine step value)
 * compact representation: represented as single object with 3 fields: lower bound, upper bound, step value
 */

val r: Range = 1 until 5 // 1, 2, 3, 4
val s: Range = 1 to 5 // 1, 2, 3, 4, 5
1 to 10 by 3 // 1, 4, 7, 10
6 to 1 by -2 // 6, 4, 2

/*
zip: can zip Seqs of different types
if diff lengths will truncate
 */
List(1, 2, 3).zip(Vector('A', 'B'))

val pairs = Vector((1, 'a'), (2, 'b'), (3, 'c'))
pairs.unzip // splits a Seq of pairs into 2 sequences consisting of 1st & 2.d elem of all pairs respectively

// Combinations

/** List all combinations of numbers x & y where x has range 1..M and y has
  * range 1..N
  */
def combinations(m: Int, n: Int) =
  (1 to m).flatMap(x =>
    (1 to n).map(y => (x, y))
  ) // Compile time type IndexedSeq
combinations(5, 6) // Runtime type Vector

/** Scalar product of 2 vectors NB: auto-decomposition: each pair of elements is
  * split into its halves which are then passed as the x & y params to the
  * lambda compiler auto decomposes the pair, puts 1st half in x & 2nd half in y
  */
def scalarProduct(a: Vector[Double], b: Vector[Double]) =
//  a.zip(b).map((x, y) => x * y).sum
  // more explicit:
//  a.zip(b).map(xy => xy._1 * xy._2).sum
  // more concise
  a.zip(b).map(_ * _).sum // runtime performance of all 3 basically equivalent
scalarProduct(Vector(1.0, 2.0, 3.0), Vector(2.0, 2.0, 2.0))

// Exercise
/** Test if number is prime: i.e. only divisible by 1 & itself
  */
def isPrime(n: Int): Boolean =
  (2 until (n / 2)).forall(n % _ != 0)

isPrime(1)
isPrime(2)
isPrime(13)
isPrime(49)
isPrime(101)
