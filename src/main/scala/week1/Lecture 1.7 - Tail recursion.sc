import scala.annotation.tailrec
@tailrec
def gcd(a: Int, b: Int): Int =
  if b == 0 then a else gcd(b, a % b)
gcd(14, 21)

def factorial(n: Int): Int =
  if n == 0 then 1 else n * factorial(n - 1)
factorial(4)

@tailrec
def factorialTailRec(n: Int, acc: Int = 1): Int =
  if n == 0 then acc
  else factorialTailRec(n - 1, n * acc)

factorialTailRec(5)
