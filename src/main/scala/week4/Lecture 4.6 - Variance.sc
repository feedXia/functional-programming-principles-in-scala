trait List[+T]
  def isEmpty = this match
    case Empty => true
    case _ => false

/*
Works on 2 different levels
1. Nothing <: T for any T, Lists are covariant => List[Nothing] <: List[T]. Ensures Empty is subtype of any list type user might give
2. List[Nothing]: conveys that there's nothing in list
 */
object Empty extends List[Nothing]