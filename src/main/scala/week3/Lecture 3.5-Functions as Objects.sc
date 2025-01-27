abstract class IntSet:
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
  def union(s: IntSet): IntSet

object Empty extends IntSet:
  override def incl(x:  Int): IntSet = NonEmpty(x, Empty, Empty)
  override def contains(x: Int): Boolean = false
  override def union(s: IntSet): IntSet = s
  override def toString = "Empty"
end Empty

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet:
  override def incl(x:  Int): IntSet =
    if x < elem then NonEmpty(elem, left.incl(x), right)
    else if x > elem then NonEmpty(elem, left, right.incl(x))
    else this

  override def contains(x: Int): Boolean =
    if x < elem then left.contains(x)
    else if x > elem then right.contains(x)
    else true

  override def union(s: IntSet): IntSet =
    left.union(right).union(s).incl(elem)

  override def toString: String = s"NonEmpty($elem, ${left.toString}, ${right.toString})"
end NonEmpty

object IntSet:
  // 3 function: create IntSets of lengths 0-2
  def apply() = Empty
  def apply(x: Int) = Empty.incl(x)
  def apply(x: Int, y: Int) = Empty.incl(x).incl(y)

IntSet()
IntSet(1)
IntSet(2, 3)