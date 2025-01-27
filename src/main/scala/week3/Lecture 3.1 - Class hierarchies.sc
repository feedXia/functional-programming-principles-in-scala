// superclass
abstract class IntSet:
  //abstract members: members not implemented
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
  def union(other: IntSet): IntSet

object IntSet:
  /**
   * Builds sets with one element
   * e.g. IntSet.singleton(elem)
   */
  def singleton(x: Int) = NonEmpty(x, Empty, Empty)

  /* Implementing sets as binary trees
  extends: Conforms to interface of IntSet, implementing all methods
  subclass of IntSet

  Use object as there's really only a single empty IntSet
  no need for many instances
  singleton object
  values: evaluates to itself
  */
  object Empty extends IntSet:
    def contains(x: Int): Boolean = false // empty set doesn't contain an element
    def incl(x: Int): IntSet = NonEmpty(x, Empty, Empty)
    def union(other: IntSet): IntSet = other
  end Empty

  // subclass of IntSet
  class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet:
    def contains(x: Int): Boolean =
      if x < elem then left.contains(x)
      else if x > elem then right.contains(x)
      else true

    def incl(x: Int): IntSet =
      if x < elem then NonEmpty(elem, left.incl(x), right)
      else if x > elem then NonEmpty(elem, left, right.incl(x))
      else this

    def union(other: IntSet): IntSet =
      /*each following union sets smaller than starting union sets
       left side gets smaller & smaller until is Empty set,
       then union is just return argument set
       => recursion terminates
       inefficient: decompose & reconstitute set multiple times

       Better way: pick elements of second set 1 by 1 & include in current set
       * Need to look inside other set elems
       * Need techniques to decompose data structures to find out what's inside
       */
      left.union(right).union(other).incl(elem)
  end NonEmpty

//  @main def birthday(name: String, age: Int) =
//    println(s"Happy birthday, $name! $age years old already")


