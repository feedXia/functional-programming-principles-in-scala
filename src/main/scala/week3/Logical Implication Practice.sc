abstract class BOOLEAN:
  def ifThenElse[T](t: => T, e: => T): T
  def ==>(b: BOOLEAN): BOOLEAN =
    ifThenElse(b, TRUE)

object TRUE extends BOOLEAN:
  override def ifThenElse[T](t: => T, e: => T): T = t

object FALSE extends BOOLEAN:
  override def ifThenElse[T](t: => T, e: => T): T = e

TRUE ==> TRUE
TRUE ==> FALSE
FALSE ==> TRUE
FALSE ==> FALSE

