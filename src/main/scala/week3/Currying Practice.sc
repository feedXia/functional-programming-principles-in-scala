/*
Exercise 1: Basic Currying
Write a curried function that takes two integers and returns their sum.
 */

val add: Int => (Int => Int) = (x: Int) => (y: Int) => x + y
add(3)(4) // 7

/*
Exercise 2: Curried Function with String Concatenation
Create a curried function that takes two Strings and concatenates them.
 */

val concat: String => String => String = (s: String) => (t: String) => s + t
concat("Hello ")("World") // "Hello World"

/*
Exercise 3: Currying with Default Parameters
Write a curried function with default values for the second parameter.
The function should take an Int and an optional Int (default to 1),
and return their product.
 */

def multiply(x: Int)(y: Int = 1): Int = x * y
multiply(4)(2) // 8
multiply(4)() // 4

/*
Exercise 4: Currying with Type InferenceWrite a curried function that accepts two arguments: a String and a Boolean. It should return a String based on the following:

If the boolean is true, it should return the string in uppercase.
If the boolean is false, it should return the string in lowercase.
 */

val format: String => Boolean => String = (s: String) =>
  (isUpper: Boolean) =>
    if isUpper then s.toUpperCase
    else s.toLowerCase

format(
  "Hello"
) // Boolean => String funtion that takes a boolean & returns "Hello" uppercase if true & lowercase if false
format("Hello")(true) // "HELLO"

/*
Exercise 5: Using Curried Function for Partial Application
Use currying to create a function that can add a fixed number to any given integer.
 */

val addFixedNumber: Int => Int => Int = (x: Int) => (y: Int) => x + y
addFixedNumber(10)(5) // 15
addFixedNumber(20)(-5) // 15

/*
Exercise 6: Currying with More Than Two Parameters
Create a curried function that takes three parameters: a String, an Int, and a Double.
The function should return a formatted string combining all the values.
 */

val formatData: String => Int => Double => String = (s: String) =>
  (i: Int) => (d: Double) => s"$s ${i.toString} ${d.toString}"

formatData("Item")(5)(20.5) // "Item 5 20.5"

/*
Exercise 7: A Curried Function with Multiple Calls
Create a curried function that takes two Ints and
returns the result of the first number raised to the power of the second.
 */

val power: Int => Int => Int = (base: Int) =>
  (exponent: Int) => Math.pow(base, exponent).toInt
power(2)(3) // 8
power(5)(2) // 25

/*
Exercise 8: Combining Functions with Currying
Create two curried functions:

One function that takes an Int and returns its square.
Another function that takes an Int and doubles it.

Then, combine them into one function that first squares the number,
then doubles the result.
 */

val square = (x: Int) => x * x
val double = (x: Int) => 2 * x

val squareDouble = (x: Int) => double(square(x))
squareDouble(3) // 18

/*
Exercise 9: A Curried Function for Subtraction
Write a curried function that performs subtraction.
It should take two parameters:
the first one being the number to subtract from,
and the second being the number to subtract.
 */

val subtract = (minuend: Int) => (subtrahend: Int) => minuend - subtrahend
subtract(10)(3) // 7
subtract(20)(5) // 15

/*
Exercise 10: Using Currying with Higher-Order Functions
Write a curried function that takes an integer n
and returns a function that multiplies its input by n.
 */

val multiplier = (n: Int) => (input: Int) => n * input
val multiplyBy2 = multiplier(
  2
) // funtion that takes an integer & multiplies it by 2
multiplyBy2(5) // 10

/*
Extra Challenge: Currying with Lists
Write a curried function that takes a List[Int] and a String,
then returns a formatted string that lists the sum of the elements in the list and the provided string.
 */

val sumListAndFormat: List[Int] => (String => String) =
  (numList: List[Int]) => (prefix: String) => s"$prefix${numList.sum}"

sumListAndFormat(List(1, 2, 3, 4))("The sum is: ")
