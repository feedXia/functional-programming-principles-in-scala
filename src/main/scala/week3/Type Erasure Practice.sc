/**
Type erasure:
 * Specific type info gets removed after compilation
 * Only generic type remains at runtime
 */
trait Character:
  def attack(character: Character): Unit =
    character.attack(this)

class Warrior extends Character:
  override def attack(warrior: Character) =
    println("Warrior attacks with sword")

class Wizard extends Character:
  override def attack(w: Character) =
    println("Wizard attacks with wand")

class Archer extends Character:
  override def attack(w: Character) =
    println("Archer attacks with bow")

/*
 * At compile time Scala knows exact types in lists,
 * but after compile the specific types are erased
 * both lists considered to be typ List[Any] at runtime
 */
val warriors: List[Warrior] = List(new Warrior(), new Warrior)
val wizards: List[Wizard] = List(new Wizard(), new Wizard())

val someCharacter: Character = warriors.head

// Example 2

// Superclass for all toys
class Toy(val name: String):

  // Every toy can use method, but the way each toy plays is different (name changes)
  def play(): Unit = println(s"Playing with $name!")

/*Specific types of types inherit from Toy class
  Each has special action
 */
class Doll(name: String) extends Toy(name):
  def dressUp(): Unit = println(s"Dressing up the $name doll!")
class Truck(name: String) extends Toy(name):
  def honk(): Unit = println(s"The $name truck honks!")

/*
 * Generic Toy Box class that can hold any toy
  T can be Doll or Truck or any subclass of Toy
  allows us to put any toy in box & perform actions on them (e.g. playing)
 * */
class ToyBox[T <: Toy]:
  private var toys: List[T] = List()

  def addToy(toy: T): Unit =
    toys = toys :+ toy

  def playWithallToys(): Unit =
    toys.foreach(_.play()) // call the play() method on every toy in box

  def getToys(): List[T] =
    this.toys

// Create toys & put in box
val doll = new Doll("Barbie")
val truck = new Truck("Hot Wheels")

val toyBox = new ToyBox[Toy]()

toyBox.addToy(doll)
toyBox.addToy(truck)

toyBox.playWithallToys()
/*
When program compiled, Scala "erases" specific toy types (Doll, Truck) at runtime
At runtime, scala doesn;t know if T is Doll or Truck or anything else, only it's some kind of Toy

Why?
 * keep program lightweight & efficient: less memory & processing power
 */

val firstToy = toyBox.getToys().head
// firstToy.dressUp() => error because Toy doesn;t know dressUp() exists

// Fix this
firstToy match {
  case d: Doll => d.dressUp()
  case t: Truck => t.honk()
  case _ => println("Unknown toy!")
}
