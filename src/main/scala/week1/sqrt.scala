package week1
import scala.annotation.tailrec

def square(x: Double) = x * x
def abs(x: Double) = if x > 0 then x else -x

def sqrt(x: Double) =
  @tailrec
  def sqrtIter(guess: Double): Double =
    if isGoodEnough(guess) then guess
    else sqrtIter(improve(guess))
  def improve(guess: Double) = (guess + x / guess) / 2
  def isGoodEnough(guess: Double) = abs(guess * guess - x) < 0.001
  sqrtIter(1.0)

@main def test = println(sqrt(2))
