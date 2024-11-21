import scala.annotation.tailrec
def improve(guess: Double, x: Double): Double = (guess + x / guess) / 2

def isGoodEnough(guess: Double, x: Double): Boolean =
  Math.abs(guess * guess - x) < 0.001

@tailrec
def sqrtIter(guess: Double, x: Double): Double =
  if isGoodEnough(guess, x) then guess
  else sqrtIter(improve(guess, x), x)

def sqrt(x: Double) = sqrtIter(1.0, x)

sqrt(4.0)

@main def test = println(sqrt(2))
