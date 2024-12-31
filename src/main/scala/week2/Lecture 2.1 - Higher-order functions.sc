import scala.annotation.tailrec
/*
 * Higher order function example
 */

def id(x: Int) = x

/** Take the sum of the integers between a & b
  */
def sumInts(a: Int, b: Int): Int =
//  if a > b then 0 else a + sumInts(a + 1, b)
  sum(x => x, a, b)

def cube(x: Int) = x * x * x

/** Take the sum of the cube of all integers between a & b
  */
def sumCubes(a: Int, b: Int): Int =
//  if a > b then 0 else cube(a) + sumCubes(a + 1, b)
  sum(x => x * x * x, a, b)

def fact(x: Int): Int =
  if x == 0 then 1 else x * fact(x - 1)

/** Take the sum of the factorials between a & b
  */
def sumFactorials(a: Int, b: Int): Int =
  if a > b then 0 else fact(a) + sumFactorials(a + 1, b)

// factor out common pattern
def sum(f: Int => Int, a: Int, b: Int): Int =
  if a > b then 0
  else f(a) + sum(f, a + 1, b)

def tailRecursiveSum(f: Int => Int, a: Int, b: Int): Int =
  @tailrec
  def loop(a: Int, acc: Int): Int =
    if a > b then acc
    else loop(a + 1, acc + f(a))
  loop(a, 0)

/*
  Anonymous function syntax
 * type of parameter can be ommited if can be inferred by compiler from context
  parameter => body
  (x: Int) => x * x * x
  (x: Int, y: Int) x + y
 */
