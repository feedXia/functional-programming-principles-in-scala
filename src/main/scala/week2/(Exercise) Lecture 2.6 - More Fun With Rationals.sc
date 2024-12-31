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
  Define a method GCD to simplify representaton of Rational class
  Private: can only be accessed inside Rational class
  Here we calculate GCD imediately so value can be used in the calculations of numer & denom
   */
  @tailrec
  private def gcd(a: Int, b: Int): Int =
    if b == 0 then a else gcd(b, a % b)
  private val g = gcd(x.abs, y)
  def numer = x
  def denom = y

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

  override def toString: String = s"${numer / g}/${denom / g}"
end Rational

/* Extension methods
 * adds new functionality to a class without changing existing functionality
 * Visible if defined in the companion object of a class, or defined or imported in the current scope
 * Can be called as if member of class
 * Can only add new members, not override existing
 * cannot refer to other class members via this: see instance from outside
 */
extension (r: Rational)
  /* An alphanumeric method with a single param can be used as an infix operator
  if delared with "infix" modifier
  this enforced a certain uniformity between usages
  not recommended to mix infix & method syntax
   */
  infix def min(s: Rational): Rational = if s.less(r) then s else r
  def abs: Rational = Rational(r.numer.abs, r.denom)

  /* Operator methods
   * with single param can be used as an infix operator
   */
  def +(s: Rational): Rational = r.add(s)
  def *(s: Rational): Rational = r.mul(s)
  def <(s: Rational): Boolean = r.less(s)


val x = Rational(1, 3)
val y = Rational(5, 7)
val z = Rational(3, 2)
x.add(y).mul(z)
x.sub(y).sub(z)

val n = Rational(42, 33)

Rational(1, 2).min(Rational(2, 3))
x + y // x.+(y)
x * y // x.*(y)
z < x // z.<(x)
y min z //y.min(z)
