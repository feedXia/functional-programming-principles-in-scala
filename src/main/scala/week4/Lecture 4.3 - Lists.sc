import scala.::
val l = Nil :: Nil :: List(Nil, Nil) :: Nil
l.length

/*
Insertion sort
* Sort tail
* Insert head in right place
* O(n^2)
 */

def insert(y: Int, ys: List[Int]): List[Int] =
  ys match
    case Nil => List(y)
    case head :: tail => if y < head then y :: ys else head :: insert(y, tail)

def isort(xs: List[Int]): List[Int] = xs match
  case Nil => Nil
  case y:: ys => insert(y, isort(ys))



isort(List(7, 3, 9, 2))