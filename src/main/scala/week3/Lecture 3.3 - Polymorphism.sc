import week3.Nil
/*
Generalising list definition using a type parameter
We have:
 * List of T, where T is the element type
 * that has 2 subclasses:
 * Cons of T: a Cons cell of type T with a head of same type T & tail of type of List of T
 * Nil of T: another subclass of List of T
 */
trait List[T]:
  // Base trait List of type T with methods
  def isEmpty: Boolean
  def head: T
  def tail: List[T]

/* Cons subclass that defines head & tail as params given: this contains impls for head & tail methods in trait List[T]
    Value pram also defines a field
  */
class Cons[T](val head: T, val tail: List[T]) extends List[T]:
  def isEmpty = false

class EmptyList[T] extends List[T]:
  def isEmpty = true
  def head = throw new NoSuchElementException("Nil.head")
  def tail = throw new NoSuchElementException("Nil.tail")

def singleton[T](elem:T) = Cons[T](elem, EmptyList[T])
singleton[Int](1)
singleton(1) // equivalent

singleton[Boolean](true)
singleton(true) // equivalent