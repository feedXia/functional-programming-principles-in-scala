import week3.Nil

// Names are uppercase to avoid conflict with predefined Scala classes
/*
Generalising list definition using a type parameter
We have:
 * List of T, where T is the element type
 * that has 2 subclasses:
 * Cons of T: a Cons cell of type T with a head of same type T & tail of type of List of T
 * Nil of T: another subclass of List of T
 */
trait LIST[T]:
  // Base trait List of type T with methods
  def isEmpty: Boolean
  def head: T
  def tail: LIST[T]


/* Cons subclass that defines head & tail as params given: this contains impls for head & tail methods in trait List[T]
    Value pram also defines a field
  */
class CONS[T](val head: T, val tail: LIST[T]) extends LIST[T]:
  def isEmpty = false

class NIL[T] extends LIST[T]:
  def isEmpty = true
  def head = throw new NoSuchElementException("Nil.head")
  def tail = throw new NoSuchElementException("Nil.tail")

def singleton[T](elem:T) = CONS(elem, NIL[T])
singleton[Int](1)
singleton(1) // equivalent
singleton[Boolean](true)
singleton(true) // equivalent


/**
 * Takes a list & an integer and selects the nth element of the list
 * Index outside range => throw IndexOutOfBoundsException
 */
def nth[T](xs: LIST[T], n: Int): T =
  if xs.isEmpty then throw IndexOutOfBoundsException("List is empty!")
  else if n == 0 then xs.head
  else nth(xs.tail, n - 1)

val myIntList = CONS(12, CONS(13, CONS(34278, CONS(0, NIL()))))
nth(myIntList, 2)
nth(CONS(1, CONS(2, CONS(3, NIL()))), 3)
