import scala.::
def sum(xs: List[Int]): Int = xs match
  case Nil    => 0
  case h :: t => h + sum(t)

def product(xs: List[Int]): Int = xs match
  case Nil    => 1
  case h :: t => h * product(t)

val nums = List(1, 2, 3, 4)
sum(nums)
product(nums)

/* pattern abstracted into generic reduceLeft
List(x1, ... xn).reduceLeft(op) - x1.op(x2). ... .op(xn)
 */

def sumReduce(xs: List[Int]) = (0 :: xs).reduceLeft(_ + _)
def productReduce(xs: List[Int]) = (1 :: xs).reduceLeft(_ * _)

extension [T](xs: List[T])
  def foldLeft[U](z: U)(op: (U, T) => U): U = xs match
    case Nil    => z
    case h :: t => t.foldLeft(op(z, h))(op)

  def reduceLeft(op: (T, T) => T): T = xs match
    case Nil    => throw IllegalAccessException("Nil.reduceLeft")
    case h :: t => t.foldLeft(h)(op)

  def foldRight[U](z: U)(op: (T, U) => U): U = xs match
    case Nil    => z
    case h :: t => op(h, t.foldRight(z)(op))

  def reduceRight(op: (T, T) => T): T = xs match
    case Nil      => throw IllegalArgumentException("Nil.reduceRight")
    case h :: Nil => h
    case h :: t   => op(h, t.reduceRight(op))
/*
 * FoldLeft
    like reduceLeft but takes accumulator as additional param
    returned when foldLeft called on empty list
    List(x1, ..., xn).foldLeft(z)(op) = z.op(x1).op ... .op(xn)
 */

def sumFold(xs: List[Int]) = xs.foldLeft(0)(_ + _)
def productFold(xs: List[Int]) = xs.foldLeft(1)(_ * _)

/*
  foldLeft & foldRight
 * equivalent for associative & commutative ops (may be difference in efficiency)
 *
 */

def concat[T](xs: List[T], ys: List[T]): List[T] =
  xs.foldRight(ys)(_ :: _)

def reverse[T](xs: List[T]): List[T] =
  xs.foldLeft(List[T]())((reversedListSoFar, currentElem) =>
    currentElem :: reversedListSoFar
  )

Nil.reverse // Nil
List(
  1
).reverse // List(1) = List(1).foldLeft(Nil)(op) = op(Nil, x) = List(x) = x :: Nil => op is :: but with operators reversed
List(1, 2).reverse // List(2, 1)

def mapFun[T, U](xs: List[T], f: T => U): List[U] =
  xs.foldRight(List[U]())((curr, acc) => f(curr) :: acc)

def lengthFun[T](xs: List[T]): Int =
  xs.foldRight(0)((_, acc) => acc + 1)

mapFun(nums, _ * 2)
lengthFun(nums)