import jdk.nashorn.internal.objects.NativeMath.abs

import scala.annotation.tailrec
class Rational(x: Int, y: Int):
  /*
  Predefined function
  Takes a condition & optional message string
  If condition passed is false, throws IllegalArgumentException with given message
  Intent: used to enforce a precondition on caller of function, called with incorrect arguments
   */
  require(y > 0, s"denominator must be positive, was $x/$y")

  /*
  Auxiliary constructors
   * methods named "this"
   * Alternate constructor that takes only a single arg
   */
  def this(x: Int) = this(x, 1)

  /*
  Throws AssertionError
  Intent: check code of function itself, bug in implementation
   */
  val z = Math.sqrt(y)
  assert(z >= 0)

  /*
  Define a method GCD to simplify representaton of Rational class
  Private: can only be accessed inside Rational class
  Here we calculate GCD imediately so value can be used in the calculations of numer & denom
   */
  @tailrec
  private def gcd(a: Int, b: Int): Int =
    if b == 0 then a else gcd(b, a % b)
  private val g = gcd(x.abs, y)
  def numer = x / g
  def denom = y / g

  // Self reference
  def less(that: Rational): Boolean =
    numer * that.denom < that.numer * denom

  // equivalent to above
  def less1(that: Rational): Boolean =
    this.numer * that.denom < that.numer * this.denom

  def max(that: Rational): Rational =
    if this.less(that) then that else this

  def add(r: Rational): Rational =
    Rational(numer * r.denom + r.numer * denom, denom * r.denom)

  def neg: Rational = Rational(-numer, denom)

  def sub(r: Rational): Rational = add(r.neg)

  def mul(r: Rational): Rational = Rational(numer * r.numer, denom * r.denom)

  def rec: Rational = Rational(denom, numer)

  def div(r: Rational): Rational = mul(r.rec)

  override def toString: String = s"$numer/$denom"
end Rational

val x = Rational(1, 3)
val y = Rational(5, 7)
val z = Rational(3, 2)
x.add(y).mul(z)
x.sub(y).sub(z)
