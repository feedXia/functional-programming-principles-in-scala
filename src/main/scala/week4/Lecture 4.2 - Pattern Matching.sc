trait Expr
case class Number(n: Int) extends Expr
case class Sum(e1: Expr, e2: Expr) extends Expr
case class Var(x: String) extends Expr
case class Prod(x: Expr, y: Expr) extends Expr

def eval(e: Expr): Int = e match
  case Number(n)   => n
  case Sum(e1, e2) => eval(e1) + eval(e2)

val expr = Sum(Number(1), Number(1))
eval(expr)

def show(e: Expr): String =
  def showSumProd(ex: Expr) = ex match
    case ex: Sum => s"(${show(ex)})"
    case _         => show(ex)

  e match
    case Number(n)   => s"$n"
    case Sum(e1, e2) => s"${show(e1)} + ${show(e2)}"
    case Var(x)      => x
    case Prod(x, y)  => s"${showSumProd(x)} * ${showSumProd(y)}"

show(expr)

val sumProdExpr = Sum(Prod(Number(2), Var("x")), Var("y")) // 2 * x + y
val prodSumExpr = Prod(Sum(Number(2), Var("x")), Var("y")) // (2 + x) * y
show(sumProdExpr)
show(prodSumExpr)

val expr1 = Prod(expr, Var("x"))
show(expr1)
