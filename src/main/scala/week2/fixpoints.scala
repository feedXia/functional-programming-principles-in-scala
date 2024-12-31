package week2

import java.lang.Math.abs

class fixpoints

val tolerance = 0.0001

def isCloseEnough(x: Double, y: Double) =
  abs((x - y) / x) < tolerance

def fixedPoint(f: Double => Double)(firstGuess: Double): Double =
  def iterate(guess: Double): Double =
    val next = f(guess)
    if isCloseEnough(guess, next) then next
    else iterate(next)
  iterate(firstGuess)

// sqrt(x) is a fixed point of function y = x / y
def sqrt(x: Double) =
  fixedPoint(y => (y + x / y) / 2)(1.0)

def averageDamp(f: Double => Double)(x: Double): Double = (x + f(x)) / 2

// Write a sqrt function using fixedPoint & averageDamp
def sqrt1(x: Double) =
  fixedPoint(averageDamp(y => x / y))(1.0)

@main def test =
  sqrt(4)
