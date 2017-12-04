package example

import org.scalatest._

class TupleSpec extends FlatSpec with Matchers {
  behavior of "A Tuple"

  it should "allows members to be accesed using _1 and so on" in {
    val t = ("Apples", "Dog")
    val fruits = t._1
    val animal = t._2
    fruits should be("Apples")
    animal should be("Dog")
  }

  it should "allow members to be accessed using destructuring" in {
    val student = ("Sean Rogers", 21, 3.5)
    val (name, age, gpa) = student
    name should be("Sean Rogers")
    age should be(21)
    gpa should be(3.5)
  }

  behavior of "A Tuple of size 2"

  it should "allow members to be swapped" in {
    val original = ("apple", 3)
    val (fruit, qty) = original
    val result = original.swap
    result._1 shouldBe qty
    result._2 shouldBe fruit
  }
}

