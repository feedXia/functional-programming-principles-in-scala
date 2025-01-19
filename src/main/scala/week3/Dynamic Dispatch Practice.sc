/*
Dynamic Dispatch/ Binding
* program decides to call the method of the actual object
* At runtime, the program will choose the method of the actual object type, even if it's being treated as a more generic type in the code.
 */
// Example 1
class Character:
  def specialMove(): Unit =
    println("Character des a basic attack")
end Character

class Warrior extends Character:
  override def specialMove(): Unit =
    println("Warrior uses sword strike!")
end Warrior

class Mage extends Character:
  override def specialMove(): Unit =
    println("Mage casts a fireball!")
end Mage

val warrior = new Warrior
val mage = new Mage

val characters = List(warrior, mage)
characters.foreach(_.specialMove())


// Example 2
class Animal:
  def makeSound(): Unit =
    println("Some generic animal sound")
end Animal

class Dog extends Animal:
  override def makeSound(): Unit =
    println("Woof! Woof!")
end Dog

class Cat extends Animal:
  override def makeSound(): Unit =
    println("Meow!")
end Cat

// Don;t specify exactly what type myPet would be until runtime.
val myPet: Animal = new Dog() // myPet is a Dog object, but it's type is Animal

// Actual object created in memory at runtime is a Dog
myPet.makeSound() // At runtime, Scala looks at actual object - Dog - and chooses the method makeSound() from the Dog class not the Animal class
