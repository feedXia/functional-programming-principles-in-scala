import scala.::

/** List Methods */

val xs = 1 :: 2 :: 3 :: Nil
val ys = List(4, 5, 6, 7)
xs.init // All elements except last, exception if list empty
xs.take(2) // first n elements, or xs if shorter than n
xs.drop(1)

// Creating new lists
xs ++ ys
xs.reverse
xs.updated(1, 200) // updates element at index n with x
xs.indexOf(2) // -1 if not in list
xs.contains(2) //same as xs.indexOf(2) >= 0

/*
Implement last
 * head: constant time complexity
 * last? takes steps proportional to length of list => linear time, much less efficient than head or tail
 */

def last[T](xs: List[T]): T = xs match
  case Nil     => throw Error("last of empty list")
  case List(x) => x
  case _ :: ys => last(ys)

last(xs)

/*
Like last, complexity is proportional to length of list
 */
def init[T](xs: List[T]): List[T] = xs match
  case Nil     => throw Error(("init of emoty list"))
  case List(_) => Nil
  case y :: ys => y :: init(ys)

/* Implement Concat
  Complexity: O(length of xs)
 */

extension [T](xs: List[T])
  def ++(ys: List[T]): List[T] = xs match
    case Nil        => ys
    case x :: xTail => x :: (xTail ++ ys)

/* Reverse
  Complexity: O(length of xs * length of xs) quadratic
  go through list xs (linear) for each element call concat (proportional to length of left list - linear)
  Array: reverse in lonear time swapping pairs of elements
  Can we do better with Lists in functional programming? Later
 */
extension [T](xs: List[T])
  def reverse: List[T] = xs match
    case Nil        => Nil
    case x :: xTail => xTail.reverse ++ List(x)

xs.reverse

/*
Exercise 1
removes nth element of xs, if n out of bounds return xs
No need for special case where n is out of bounds as logic will naturally mean nothing will be removed! (WOAH)
 */

def removeAt[T](n: Int, xs: List[T]): List[T] = xs match
  case Nil => Nil
  case head :: tail =>
    if n == 0 then tail
    else head :: removeAt(n - 1, tail)

removeAt(3, xs ++ ys)
removeAt(1, List('a', 'b', 'c', 'd'))

/*
Exercise 2
flatten nested list structures
return single list with no nested lists, containing just elements with no embedded sublists
 */

def flatten(xs: List[Any]): List[Any] = xs match
  case Nil => Nil
  case h :: t =>
    h match // h could have embedded sublists & so could t
      case List(x) =>
        x :: flatten(t)
      case l: List[Any] =>
        flatten(l) ++ flatten(t)
      case _ =>
        h :: flatten(t)

flatten(List("just me!"))
flatten(List(List(1)))
flatten(List(List(1), List(2), List(3)))
flatten(List(List('a')))
flatten(List(List('a', List('b'))))
flatten(List(List(1, 1), 2, List(2, List(5, 8))))

/* Pattern matcing can be done on very general types - including any
 Useful for code that is weakly typed/ many different types works in homogeneous fashion
 */
def flattenMartin(xs: Any): List[Any] = xs match
  case Nil => Nil
  case h :: t => flattenMartin(h)++flattenMartin(t)
  case _ => xs :: Nil // If xs something else, i.e. not list, turn it into single element list

flattenMartin(List(List(1, 1), 2, List(2, List(5, 8))))
flattenMartin("hello I am not a list!")
flattenMartin(("not a list", List("inside list")))