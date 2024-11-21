/*
Substitution model
All evaluation does is reduce an expression to a value
Can be applied to all expressions as long as no side effects (purely functional)
1. Evaluate all function args: left -> right
2. Replace function application by function's RHS (body of function)
3. At the same time: replace formal params of function by actual args
 */

/*
Termination
Q: Does every expression terminate?
A: No e.g. def loop: Int = loop

Evaluation strategy
Call by value: Previous evaluation strategy: reduce args to values, rewrite function application
 * evaluate things before pass them so pass only values not full expressions
 * advantage: evaluates every function arg only once

Val by name: Alternative strategy: apply function to unreduced values
 * if a parameter is unused in the evaluation of function body, then function argument is not evaluated at all

both strategies reduce to same final values as long as:
1. reduced expressions consists of pure expressions
both evaluations terminate
 */

/** Lecture 1.3 - Evaluation Strategies and Termination What if termination is
  * not guaranteed?
  *
  * THEOREM If a CBV evaluation terminates => CBV evaluation terminates too
  * Inverse is not true: there exists expressions that terminate under CBN that
  * do not under CBV
  */

/*
Scala normally uses CBV
Can explicitly trigger CBN: if the type of a function parameter startes with =>
 */

def constOne(x: Int, y: => Int) = 1

def and0(x: Boolean, y: Boolean): Boolean = (x, y) match {
  case (true, true)   => true
  case (true, false)  => false
  case (false, true)  => false
  case (false, false) => false
}

def and(x: Boolean, y: => Boolean): Boolean = if x then y else false

def or0(x: Boolean, y: Boolean): Boolean = (x, y) match {
  case (true, true)   => true
  case (true, false)  => true
  case (false, true)  => true
  case (false, false) => false
}

def or(x: Boolean, y: Boolean): Boolean = if x then true else y
