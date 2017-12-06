package example

import org.scalatest._

class ClassWithName(val name: String)

class Point(val x: Int, val y: Int) {
  override def toString(): String = "(" + x + ", " + y + ")"
}

object PointService {
  def create(x: Int, y: Int): Point = {
    val pt = new Point(x,y)
    return pt
  }
}

class Movie(val name: String, val year: Short)

object Movie {
  def academyAwardBestMovieForYear(x: Short) = {
    x match {
      case 1930 ⇒ Some(new Movie("All Quiet On the Western Front", 1930))
      case 1931 ⇒ Some(new Movie("Cimarron", 1931))
      case 1932 ⇒ Some(new Movie("Grand Hotel", 1932))
      case _    => None
    }
  }
}

class Person(val name: String, private val alterEgo: String)

object Person {
  def secretRevealed: Person => String = x => x.alterEgo
}

class HelloSpec extends FlatSpec with Matchers {
  behavior of "Scala Primitives"

  "The Hello object" should "say hello" in {
    Hello.greeting shouldEqual "hello"
  }

  "The primitive true" should "be true" in {
    true should be(true)
  }

  "The number 4" should "equal 4" in {
    val v1 = 4
    v1 shouldEqual 4
  }

  "The number 2" should "equal 1 + 1" in {
    assert(2 == 1 + 1)
  }

  "class" should "provide access to property once instantiated" in {
    val input = new ClassWithName("Gandalf")
    val result = input.name
    val expected = "Gandalf"
    result should equal(expected)
  }

  behavior of "PointService.create"

  "PointService.create" should "return Point" in {
    val x: Int = 3
    val y: Int = 5
    val result = PointService.create(x,y)
    result.x shouldBe 3
    result.y should equal(5)
  }

  "PointService.create" should "be an instance of Point" in {
    val x: Int = 3
    val y: Int = 5
    val result = PointService.create(x,y)
    result shouldBe a[Point]
  }

  behavior of "object"

  it should "create singleton" in {
    val a = OptionExample
    val b = OptionExample
    val c = b
    c eq a should be(true)
    OptionExample eq c should be(true)
  }

  behavior of "Companion Object"

  it should "contain factory methods" in {
    Movie.academyAwardBestMovieForYear(1932).get.name should be("Grand Hotel")
  }

  // This does not work on the REPL ??
  it should "be able to access private members" in {
    val clark = new Person("Clark Kent", "Superman")
    val result = Person.secretRevealed(clark)
    result should be("Superman")
  }
}

