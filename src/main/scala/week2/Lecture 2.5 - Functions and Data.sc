class Rational(x: Int, y: Int):
  def numer = x
  def denom = y

  // Methods: inside of class
  def add(r: Rational) = Rational(
    numer * r.denom + r.numer * denom,
    denom * r.denom
  )
  def neg = Rational(-numer, denom)
  def sub(r: Rational) = add(r.neg)
  def mul(r: Rational) = Rational(numer * r.numer, denom * r.denom)
  def div(r: Rational) = Rational(numer * r.denom, denom * r.numer)
  def equal(r: Rational) = numer * r.denom == denom * r.numer
  // override: Every class already defines a toString method, explicitly changing defualt behaviour
  override def toString = s"$numer/$denom"

end Rational

val r = Rational(1, 2)
r.numer
r.denom

val x = Rational(1, 3)
val y = Rational(5, 7)
val z = Rational(3, 2)
x.add(y).mul(z)
x.sub(y).sub(z)
x.add(y.neg).add(z.neg) //equivalent to above

// Functions: outside of class
def addRational(r: Rational, s: Rational): Rational =
  Rational(r.numer * s.denom + s.numer * r.denom, r.denom * s.denom)

def makeString(r: Rational): String =
  s"${r.numer}/${r.denom}"

makeString(addRational(Rational(1, 2), Rational(2, 3)))
