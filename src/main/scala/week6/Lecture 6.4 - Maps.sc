val romanNumerals = Map("I" -> 1, "V" -> 5, "X" -> 10)
val capitalOfCountry = Map("US" -> "Washington", "Switzerland" -> "Bern")

/*
Subtype of Iterables
Therefore supports same coollection ops e.g. map
 */

val countryOfCapital = capitalOfCountry.map((x, y) => (y, x))

/* ->: a method defn, implemented as extension method in Predef, obj implicitly imported in every Scala program
extension [K, V] (k: K)
  def -> (v: V) = (k, v)
 */

/*
Maps are Iterables
and also functions: Map[key, value] extends function type key => value, so can be used everywhere functions can
so can be applied to key arguments
 */

capitalOfCountry("US")

// Applying map to non-existing key gives error
capitalOfCountry("Andorra") //NosuchElementException

// get: Option value
capitalOfCountry.get("US")
capitalOfCountry.get("Andorra")

/* Option Type
single type param A, which is covariant
 */
//trait Option[+A]
//
//case class Some[+A](value: A) extends Option[A]
//object None extends Option[Nothing]

// Since options are defined as case classes, they can be decomposed using pattern matching
def showCapital(country: String) = capitalOfCountry.get(country) match
  case Some(capital) => capital
  case None          => "missing data"

showCapital("US")
showCapital("Andorra")

/*
Why option and not null?
If any value can be null, never know before hand whether certain
ops on value are defined or not, if value is null will get NullPointerException

option is safer because it forces you to handle both cases - safer
 */

// Updating maps
capitalOfCountry + ("South Korea" -> "Seoul") // creates new map with updated key if exists else appends key value
capitalOfCountry ++ List(
  "China" -> "Beijing",
  "United Kingdom" -> "London"
) // creates new map with all key/values from collection updated/added via '+'

/*
 * Smaller maps (≤ 4): single object, copy whole object & update
 * larger maps: similar to vector scheme, array of arrays with shallow depth up to 5, each array contains kv pairs, uses hash function to select sub-array/elem
  update - log(n) (n: depth of tree) update of sub -arrays
 */

// Operations from relational calculus
// orderBy from SQL can be expressed using sortWith and sorted
val fruit = List("apple", "pear", "orange", "pineapple")
fruit.sortWith(_.length < _.length) // sort by length of strings
fruit.sorted // sorts with standard comparison func, for strings it's lexicographic ordering

// groupBy: partitions a collection into a map of collections according to a discriminator func f
fruit.groupBy(_.head)

/*
Map example
polynomials can be seen as map from exponents to coefficients
 */

// e.g. x^3 - 2x + 5
Map(0 -> 5, 1 -> 2, 3 -> 1)

// design a Polynom class that represents polynomials as maps
/* recall maps are PARTIAL FUNCTIONS: applying a map to kv in map(key) could lead to exception if k not in map
this is inconvenient as can ntreat missing coefficients as cases where it's 0
operation withDefaultValue turns a map into a total function
 */
val cap1 = capitalOfCountry.withDefaultValue("<unknown>")
cap1("Andorra")

class Polynom(nonZeroTerms: Map[Int, Double]):

  val terms = nonZeroTerms.withDefaultValue(0.0)

  def +(other: Polynom): Polynom = Polynom(this.terms ++ other.terms)

  override def toString = terms.toString

val x = Polynom(Map(0 -> 2, 1 -> 3, 2 -> 1))
