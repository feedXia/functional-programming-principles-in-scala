import java.util.Date
import javax.smartcardio.Card

/*
Modelling data with class hierarchies

Classes
- essentially bundles of functons operating on some common values represented as fields
- useful abstraction: allows encapsulation of data
 */

/*
Composing & decomposing pure data
- W/o associated functions
- Case classes & pattern matching work well
 */

trait ExprOld // Pure data defintiions like these are called algebraic data types (ADTs)
object ExprOld:
  case class VarOld(s: String) extends ExprOld
  case class NumberOld(n: Int) extends ExprOld
  case class SumOld(e1: ExprOld, e2: ExprOld) extends ExprOld
  case class ProdOld(e1: ExprOld, e2: ExprOld) extends ExprOld

ExprOld.NumberOld(1)
//Or
import Expr.Sum
import ExprOld.*

import javax.crypto.KeyAgreement
SumOld(NumberOld(1), NumberOld(2))

/*
Enum
- Enumerates all cases of an ADT & nothing else
- A shorthand for hierarchies of case classes
- A way to define data types accepting alternative values
- Can be comprised of parameterised and simple cases at the same time
- Typically used for pure data, where all ops on data defined elsewhere
 */

enum Expr: // Equivalent to above example, syntactic sugar avoids "class", "extends Expr"
  case Var(s: String)
  case Number(n: Int)
  case Sum(e1: Expr, e2: Expr)
  case Prod(e1: Expr, e2: Expr)

import Expr.*

def show(e: Expr): String =
  def showP(e: Expr) = e match
    case e: Sum => s"(${show(e)})"
    case _      => show(e)

  e match
    case Expr.Var(x)       => x
    case Expr.Number(n)    => n.toString
    case Expr.Sum(e1, e2)  => s"${show(e1)} + ${show(e2)}"
    case Expr.Prod(e1, e2) => s"${showP(e1)} * ${showP(e2)}"

enum Colour:
  case Red
  case Green
  case Blue

enum DayOfWeek:
  case Monday, Tuesday, Wednesday, THursday, Friday, Saturday, Sunday

import DayOfWeek.*

def isWeekend(day: DayOfWeek): Boolean = day match
  case Saturday | Sunday => true
  case _                 => false

enum Direction(val dx: Int, val dy: Int):
  case Right extends Direction(1, 0)
  case Up extends Direction(0, 1)
  case Left extends Direction(-1, 0)
  case Down extends Direction(0, -1)

  def leftTurn = Direction.values((ordinal + 1) % 4)
end Direction

val r = Direction.Right
val u = r.leftTurn
val v = (u.dx, u.dy)

/*
Enumeration classes that pass params have to use explicit "extends"
Compiler defined helper methods in class
- ordinal: ordinal value of enum case, starts at 0 numbered consecutively
Compiler defined helper methods in companion object:
- values: immutable array in companion object of enum containing all enum values
- valueOf: gets enum case conrresponding to a string which is the name of the case
Only simple cases have ordinal numbers & show up in "values", parameterised cases do not
 */

// Enum expanded by compiler to roughly
//abstract class Direction1(val dx: Int, val dy: Int):
//  def leftTurn = Direction1.values((ordinal + 1) % 4)
//object Direction1:
//  val Right = new Direction1(1, 0) {}
//  val Up = new Direction1(0, 1) {}
//  val Left = new Direction1(-1, 0) {}
//  val Down = new Direction1(0, -1) {}
//end Direction1



/*
ADTS & enums are particularly useful for modelling tasks where
one needs to define a large number of data types without attaching operations
 */

// E.g. payment methods
enum PaymentMethod:
  case CreditCard(kind: Card, holder: String, number: Long, expired: Date)
  case PayPal(email: String)
  case Cash

enum Card:
  case Visa, Mastercard, Amex