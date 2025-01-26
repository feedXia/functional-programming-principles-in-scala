// CBN: parameter evaluated every time it is used inside the function, not immediately
def printTwiceCBN[T](t: => T): Unit =
  println(t)
  println(t)

printTwiceCBN(
  // Following expression is evaluated twice
  {
    println("doing something expensive...")
    42
  }
)

// Expression evaluated first before function is called & only result is passed on
def printTwiceCBV[T](t: T): Unit =
  println(t)
  println(t)

printTwiceCBV(
  // Evaluated first before function runs
  {
    // Side effect: prints during expression evaluation
    println("doing something expensive...")
    // Evaluates to 42
    42 // Passed to function as arg t
  }
)
