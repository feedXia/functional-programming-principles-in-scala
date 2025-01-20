package week3

/*
Class hierarchy representing lists of integers
 */


trait IntList

/*
abbreviation val head: parameters also taken as fields of the class
handy way to define simultaneously parameters & fields of calss
equivalent to:
* defining parameters by some other names then defined 2 value definitions

class Cons(_head: Int, _tail: IntList): extends IntList:
  val head = _head
  val tail = _tail

where _head, _tail otherwise unused names
*/
class Cons(val head: Int, val tail: IntList) extends IntList

// empty list
class Nil() extends IntList

// A list is either empty list or a Cons object