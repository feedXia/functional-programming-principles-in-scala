abstract class Boolean: // extends AnyVal:
  def ifThenElse[T](t: => T, e: => T): T
  def &&(x: => Boolean): Boolean = ifThenElse(x, FALSE)
  def ||(x: => Boolean): Boolean = ifThenElse(TRUE, x)
  def unary_!(): Boolean = ifThenElse(FALSE, TRUE)
  def ==(x: => Boolean): Boolean = ifThenElse(x, x.unary_!())
  def !=(x: => Boolean): Boolean = ifThenElse(x.unary_!(), x)

  extension (x: Boolean)
    /*
    Implementation operator: true if A implies B
     */
    def ==>(y: Boolean): Boolean =
      x.ifThenElse(y, TRUE)

object TRUE extends Boolean:
  override def ifThenElse[T](t: => T, e: => T): T = t

object FALSE extends Boolean:
  override def ifThenElse[T](t: => T, e: => T): T = e

TRUE && TRUE // Calls TRUE ifTE implementation, returns t(then) part

TRUE || FALSE
TRUE || TRUE
FALSE || TRUE
FALSE || FALSE

TRUE.unary_!()
FALSE.unary_!()

TRUE == FALSE
TRUE == TRUE
FALSE == TRUE
FALSE == FALSE

TRUE != FALSE
TRUE != TRUE
FALSE != TRUE
FALSE != FALSE

TRUE ==> TRUE
TRUE ==> FALSE
FALSE ==> TRUE
FALSE ==> FALSE

