/* Do not use standard numerical classes in this impl
Rather implement sub-object & sub-class
 */
abstract class Nat: // Natural numbers: numbers non-zero
  def isZero: Boolean
  def predecessor: Nat
  def successor: Nat
  def +(that: Nat): Nat
  def -(that: Nat): Nat

  // No need for toString as already implemented on Objects
end Nat

object Zero extends Nat:
  override def isZero: Boolean = true

  override def predecessor: Nat = throw new NoSuchElementException(
    "Natural number cannot be less than Zero"
  )

  override def successor: Nat = Succ(Zero)

  override def +(that: Nat): Nat = that

  override def -(that: Nat): Nat =
    if that.isZero then Zero
    else
      throw new UnsupportedOperationException("Result will be less than Zero")

  override def toString = "Zero"
end Zero

// for n, Succ(n) is n + 1, class for strictly positive numbers
class Succ(n: Nat) extends Nat:
  override def isZero: Boolean = false

  override def predecessor: Nat = n

  override def successor: Nat = Succ(this)

  override def +(that: Nat): Nat = Succ(n + that)

  override def -(that: Nat): Nat =
    if that.isZero then this
    else n - that.predecessor

  override def toString = s"Succ($n)"
end Succ

Zero.isZero
val two = Succ(Succ(Zero))
val one = Succ(Zero)

two + one
two - one
one - two