def isPrime(n: Int) =
  (2 to n / 2).forall(n % _ != 0)
/*
Given a positive integer n, find all pairs of positive ints i & j
1 ≤ j < i < n s.t. i + j is prime
 */
def sumPrime(n: Int) =
  (2 until n)
    .flatMap(i => (1 until i).map(j => (i, j)))
    .filter((x, y) => isPrime(x + y))

// NB: flatten: equivalent to applying fold right e.g. foldRight(Seq[Int]())(_ ++ _)

// For-Expression Example
case class Person(name: String, age: Int)

val persons =
  List(Person("Alice", 11), Person("Bob", 22), Person("Charlie", 33))

/*
  Similar to loops in imperative languages,
  except builds list of the results instead of working by side effects
 */
for p <- persons if p.age > 20 yield p.name
persons.filter(_.age > 20).map(_.name)

def sumPrimeWithFor(n: Int) =
  for
    i <- (2 until n)
    j <- (1 until i)
    if isPrime(i + j)
  yield (i, j)

def scalarProduct(xs: List[Double], ys: List[Double]) =
  (for (x,y) <- xs.zip(ys) yield x * y).sum
