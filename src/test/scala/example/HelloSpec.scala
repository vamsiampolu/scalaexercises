package example

import org.scalatest._

object OptionExample {
  def mayHaveValue: Boolean => Option[String] = flag => if(flag) Some("Found value") else None

  def mayHave3: Boolean => Option[Int] = flag => if(flag) Some(3) else None
}

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

  behavior of "Option"

  it should "have Some('value')" in {
    val someValue: Option[String] = Some("value")
    someValue should be(Some("value"))
  }

  it should "be None if no value is supplied" in {
    val emptyValue: Option[String] = None
    emptyValue should be(None)
  }

  behavior of "getOrElse"

  it should "retrieve value if it exists" in {
    val goodValue = OptionExample.mayHaveValue(true) getOrElse "None"
    goodValue should equal("Found value")
  }

  it should "retrieve default value if None" in {
    val sadValue = OptionExample.mayHaveValue(false) getOrElse "Nine"
    sadValue should equal("Nine")
  }

  behavior of "isEmpty"

  it should "when applied on Some('Value') be false" in {
    val result = OptionExample.mayHaveValue(true)
    result.isEmpty should be(false)
  }

  it should " when applied on None be true" in {
    val result = OptionExample.mayHaveValue(false)
    result.isEmpty should be(true)
  }

  behavior of "Option[String]"

  it should "pattern matching be FoundValue" in {
    val input = OptionExample.mayHaveValue(true)
    val result = input match {
      case Some(v) => v
      case None => "None"
    }
    result should be("Found value")
  }

  it should "pattern matching be None" in {
    val input = OptionExample.mayHaveValue(false)
    val result = input match {
      case Some(v) => v
      case None => "None"
    }
    result should be("None")
  }

  it should "work with map" in {
    val happy_input = OptionExample.mayHave3(true)
    val sad_input = OptionExample.mayHave3(false)

    val happy = happy_input map { _ * 1.5 }
    happy should be(Some(4.5))

    val sad = sad_input map { _ * 1.5 }
    sad should be(None)
  }

  it should "be unboxed out of Some(Int) using fold" in {
    val input = OptionExample.mayHave3(true)
    val result = input.fold(1)(_ * 5)
    result should be(15)
  }

  it should "return default value on fold if None" in {
    val input = OptionExample.mayHave3(false)
    val result = input.fold(1)(_ * 5)
    result should be(1)
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

