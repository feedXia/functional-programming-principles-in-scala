import scala.annotation.tailrec
// Summation functions
def sum(f: Int => Int, a: Int, b: Int): Int =
  if a > b then 0 else f(a) + sum(f, a + 1, b)

/*
  Note:
  a & b gets passed unchanged from sumInts and sumCubes into sum
  whenever there's a common pattern like this: ask yourself is it possible to eliminate repeatition?
   - get rid of these params?
 */
def sumInts(a: Int, b: Int) = sum(x => x, a, b)
def sumCubes(a: Int, b: Int) = sum(x => x * x * x, a, b)
def sumFactorials(a: Int, b: Int) = sum(fact(_), a, b)

@tailrec
def fact(x: Int, acc: Int = 0): Int =
  if x == 0 then acc else fact(x - 1, acc + x)

// Rewrite sum
/** @param f
  *   single parameter
  * @return
  *   returns a function that takes 2 Int's (bounds a & b) & gives back the
  *   final value
  */
def sum2(f: Int => Int): (Int, Int) => Int =
  // f has to return a function, call this sumF
  def sumF(a: Int, b: Int): Int =
    if a > b then 0
    else f(a) + sumF(a + 1, b)
  sumF
// Now sum is a function that returns another function

// Rewrite
def sumInts2 = sum2(x => x) // : (Int, Int) => Int
def sumCubes2 = sum2(x => x * x * x)
def sumFactorials2 = sum2(fact(_))

// These functions can then be applied
sumCubes2(1, 10) + sumFactorials2(10, 20)
def cube(x: Int) = x * x * x
sum2(cube)(1, 10)

/* Shorter syntax without nested sumF function
Implicitly defines:
 * a function that takes a single parameter f
 * returns a function that takes 2 following parameters a & b
 * but don't have to invent name or inner function
 */
def sum3(f: Int => Int)(a: Int, b: Int): Int =
  if a > b then 0 else f(a) + sum3(f)(a + 1, b)

// We can avoid the intermediate functions sumInts, sumCubes, sumFactorials

sum3(cube)(1, 10)

def product(f: Int => Int)(a: Int, b: Int): Int =
  if a > b then 1 else f(a) * product(f)(a + 1, b)

product(x => x * x)(1, 5)

def factorial(x: Int): Int =
  product(n => n)(1, x)

factorial(3)

// Keeps all values need to generalise as parameters
/** @param f:
  *   map function, applies to every element
  * @param combine:
  *   reduce function, takes 2 results of map & combines them into a single
  * @param baseCase:
  *   thing to return in case base is empty
  * @param a:
  *   second function parameter, lower bound of interval
  * @param b:
  *   second function parameter, upper bound of interval
  * @return
  */
def mapReduce(f: Int => Int, combine: (Int, Int) => Int, baseCase: Int)(
    a: Int,
    b: Int
): Int =
  def recur(a: Int): Int =
    if a > b then baseCase
    else combine(f(a), recur(a + 1))
  recur(a)

def sum4(f: Int => Int) = mapReduce(f, (x, y) => x + y, 0)

sum4(factorial)(1, 5)

def product1(f: Int => Int) = mapReduce(f, (x, y) => x * y, 1)
product1(factorial)(1, 3)
product1(identity)(1, 6)
