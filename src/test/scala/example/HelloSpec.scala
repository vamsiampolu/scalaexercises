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

  it should "have Some('value')" in {
    val someValue: Option[String] = Some("value")
    someValue should be(Some("value"))
  }

  it should "be None if no value is supplied" in {
    val emptyValue: Option[String] = None
    emptyValue should be(None)
  }

  "getOrElse" should "retrieve value if it exists" in {
    val goodValue = OptionExample.mayHaveValue(true) getOrElse "None"
    goodValue should equal("Found value")
  }

  "getOrElse" should "retrieve default value if None" in {
    val sadValue = OptionExample.mayHaveValue(false) getOrElse "Nine"
    sadValue should equal("Nine")
  }

  "isEmpty method on Some('Value')" should "be false" in {
    val result = OptionExample.mayHaveValue(true)
    result.isEmpty should be(false)
  }

  "isEmpty method on None" should "be true" in {
    val result = OptionExample.mayHaveValue(false)
    result.isEmpty should be(true)
  }

  "Option[String]" should "pattern matching be FoundValue" in {
    val input = OptionExample.mayHaveValue(true)
    val result = input match {
      case Some(v) => v
      case None => "None"
    }
    result should be("Found value")
  }

  "Option[String]" should "pattern matching be None" in {
    val input = OptionExample.mayHaveValue(false)
    val result = input match {
      case Some(v) => v
      case None => "None"
    }
    result should be("None")
  }

  "Option[String]" should "work with map" in {
    val happy_input = OptionExample.mayHave3(true)
    val sad_input = OptionExample.mayHave3(false)

    val happy = happy_input map { _ * 1.5 }
    happy should be(Some(4.5))

    val sad = sad_input map { _ * 1.5 }
    sad should be(None)
  }

  "Option[String]" should "be unboxed out of Some(Int) using fold" in {
    val input = OptionExample.mayHave3(true)
    val result = input.fold(1)(_ * 5)
    result should be(15)
  }

  "Option[String]" should "return default value on fold if None" in {
    val input = OptionExample.mayHave3(false)
    val result = input.fold(1)(_ * 5)
    result should be(1)
  }

  "object" should "create singleton" in {
    val a = OptionExample
    val b = OptionExample
    val c = b
    c eq a should be(true)
    OptionExample eq c should be(true)
  }

  "Companion Object" should "contain factory methods" in {
    Movie.academyAwardBestMovieForYear(1932).get.name should be("Grand Hotel")
  }

  // This does not work on the REPL ??
  "Companion Object" should "be able to access private members" in {
    val clark = new Person("Clark Kent", "Superman")
    val result = Person.secretRevealed(clark)
    result should be("Superman")
  }

  "Tuple members" should "be accesed using _1 and so on" in {
    val t = ("Apples", "Dog")
    val fruits = t._1
    val animal = t._2
    fruits should be("Apples")
    animal should be("Dog")
  }

  "Tuple members" should "be accessed using destructuring" in {
    val student = ("Sean Rogers", 21, 3.5)
    val (name, age, gpa) = student
    name should be("Sean Rogers")
    age should be(21)
    gpa should be(3.5)
  }

  "Tuple of size 2" should "allow members to be swapped" in {
    val original = ("apple", 3)
    val (fruit, qty) = original
    val result = original.swap
    result._1 shouldBe qty
    result._2 shouldBe fruit
  }

  "Lambda" should "work" in {
    def fn = (x:Int) => x + 1
    val fn_no_sugar = new Function1[Int, Int] {
      def apply(a: Int) = {
        a + 1
      }
    }
    val result = fn(1)
    result shouldBe  2
  }

  "Lambda" should "use closures" in {
    val y = 2
    def fn = (x: Int) => {
      x + y
    }
    val result = fn(4)
    result shouldBe 6
  }

  "Lambda" should  "accept functions" in {
    val x = 2
    val fn = (y: Int) => { x + y + 1}
    val fn2 = (a: Int, closure:Int => Int) => {
      closure(a)
    }
    val result = fn2(3, fn)
    result shouldEqual 6
  }

  // without syntax sugar
  "Lambda" should "return functions" in {
    val x = 2
    val fn = (y: Int) => {
      // return value has been desugared
      new Function1[Int, Int] {
        def apply(z:Int) = {
          x + y + z
        }
      }
    }
    val fn2 = fn(3)
    val result = fn2(1)
    result should be(6)
  }

  // with syntax sugar
  "Lambda" should "return functions2" in {
      val x = 2
      val fn = (y:Int) => { (z:Int) => x + y + z }
      val fnb = fn(3)
      val result = fnb(1)
      result should be(6)
  }

  "Lambda" should "work with isInstanceOf" in {
    val fn = (y:Int) => y + 1
    fn.isInstanceOf[Function1[Int, Int]] should be(true)
  }

  "Lambda" should "work with shouldBe a " in {
    val fn = (y:Int) => y + 1
    fn shouldBe a[Function1[_, _]]
  }

  "Lambda" should "be passed to map" in {
    def makeUpper: List[String] => List[String] = xs => xs map {
      _.toUpperCase
    }

    val a = List("abc", "xyz", "123")
    val b = makeUpper(a)
    val expectedB = List("ABC", "XYZ", "123")

    b should be(expectedB)

    def makeWhateverYouLike(xs: List[String], sideEffect: String => String) = {
      xs map sideEffect
    }

    val c = makeWhateverYouLike(b, { x => x.toLowerCase })
    c should be(a)

    val d = List("John", "Mark")
    def myName(name:String) = s"My name is $name"
    val expectedE = List("My name is John", "My name is Mark")
    val e = makeWhateverYouLike(d, myName)

    List("Scala", "Erlang", "Clojure") map (_.length) should be(
      List(5,6,7)
)
  }

  // equal does reference checking
  "List" should "be comparable using ==" in {
    val a = List("abc","xyz")
    val b = List("abc", "xyz")
    a == b should be(true)
  }

  "List" should "not be comparable using equal" in {
    val a = List("abc","xyz")
    val b = List("abc", "xyz")
    a eq b should be(false)
  }

  "List" should "use head, headOption and tail" in {
    val a = List("a", "b", "c")
    val h = a.head
    val sh = a.headOption
    h should be("a")
    sh should be (Some("a"))

    // Mini revision
    // Option can use map and fold
    val ush = sh.map { (x:String) => x.toUpperCase }
    ush should be(Some("A"))
    val fush = ush.fold("") { (x:String) => x.toLowerCase }
    fush should be("a")

    val t = a.tail
    t should be(List("b", "c"))

    val t2 = t.tail
    t2 should be(List("c"))
  }

  "List" should "allow access to elements using indices" in {
    val list = List("a", "b", "c", "d", "e")
    list(0) should be("a")
    list(2) should be("c")
    list(4) should be("e")

    intercept[IndexOutOfBoundsException] {
      println(list(5))
    }
 }

  "List" should "have utility methods" in {
    val a = List(1,2,3,4,5)
    val result = a.reverse
    val expected = List(5,4,3,2,1)
    result should be(expected)
    result.length should be(5)
  }

  "List" should "allow filter method" in {
    val a = List(1,3,5,7,9)
    val result = a filter { (num:Int) => num % 3 == 0 }

    result should be(List(3,9))
  }

  "List" should "have foldLeft and reduceLeft for aggregation" in {
    val a = List(1,3,5,7)
    val result = a.reduceLeft(_ + _)
    val expected = 16
    result should be(expected)

    val resultFold = a.foldLeft(0)(_ + _)
    resultFold should be(expected)
  }

  "List" should "be created from a Range by invoking toList" in {
    val a = 1 to 5
    val result = a.toList
    val expected = List(1,2,3,4,5)
    result should be(expected)

    val b = 1 until 5
    val result2 = b.toList
    val expected2 = List(1,2,3,4)
    result2 should be(expected2)
  }

  "List" should "allow prepending with the :: operator" in {
    val a = List(3)
    val b = 2 :: a
    val c = 1 :: b

    c should be(List(1,2,3))
  }

  "List" should "allow prepending to Nil" in {
    // Nil is treated as empty list
    val a = Nil
    val result = 1 :: a
    result should be(List(1))
  }

  "List" should "allow concat with ::: operator" in {
    val a = List(1)
    val b = List(2,3)

    val result = a ::: b
    val expected = List(1,2,3)

    result should be(expected)
  }

  "List" should "allow Nil to be concated to List" in {
    val a = List(1,2)
    val b = Nil

    val result = a ::: b
    val expected = List(1,2)

    result should be(expected)
  }

  "List" should "reuse their tails" in {
    val a = List(3)
    val b = 2 :: a
    val c = 1 :: b

    c.tail should be(b)
    b.tail should be(a)
    a.tail should be(Nil)
  }

  "Map" should "work with shorthand notation" in {
    val result = Map(
      ("a", 1),
      ("b", 2),
      ("c", 3)
    )

    val expected = Map(
      "a" -> 1,
      "b" -> 2,
      "c" -> 3
    )

    result should be(expected)
  }

  "Map" should "provide value when apply method is invoked with a key" in {
    val expected = "New York"
    val input = Map(
      "FL" -> "Florida",
      "CA" -> "California",
      "MI" -> "Michigan",
      "TX" -> "Texas",
      "NY" -> expected
    )

    // apply is an unsafe operation
   val result = input apply { "NY" }
   result should be(expected)

   // it will throw an exception if you lookup non-existant key
   intercept[NoSuchElementException] {
    input apply { "WI" }
   }
  }

  "Map" should "values can be accessed directly" in {
    val expected = "New York"
    val input = Map(
      "FL" -> "Florida",
      "CA" -> "California",
      "MI" -> "Michigan",
      "TX" -> "Texas",
      "NY" -> expected
    )

    // direct access is an unsafe operation
    val result = input("NY")
    result should be(expected)

    // it will throw an exception if you lookup non-existant key
    intercept[NoSuchElementException] {
      input("WI")
    }
  }

  "Map" should "safely retrieve value with get method" in {
    val expected = "New York"
    val input = Map(
      "FL" -> "Florida",
      "CA" -> "California",
      "MI" -> "Michigan",
      "TX" -> "Texas",
      "NY" -> expected
    )

    val result = input get("NY")
    result should be(Some(expected))

    val noneValue = input get("WI")
    noneValue should be(None)
  }

  "Map" should "safely retrieve with getOrElse" in {
    val expected = "New York"
    val expected2 = "missing value"

    val input = Map(
      "FL" -> "Florida",
      "CA" -> "California",
      "MI" -> "Michigan",
      "TX" -> "Texas",
      "NY" -> expected
    )

    val result = input getOrElse("NY", expected2)
    result should be(expected)

    val noneValue = input getOrElse("WI", expected2)
    noneValue should be(expected2)
  }

  "Map" should "be allowed to have one fallback for any missing keys" in {
    val expected = "Puerto Rico"
    val input = Map(
      "FL" -> "Florida",
      "CA" -> "California",
      "MI" -> "Michigan",
      "TX" -> "Texas",
      "NY" -> "New York"
    ) withDefaultValue expected

    // this makes direct access and apply safe operations
    input("WI") should be(expected)
    input apply { "WI" } should be(expected)
    // however, it does not affect safe operations such as get
    input get("WI") should be(None)
  }

  "Map" should "provide its size property" in {
    val input = Map(
      "FL" -> "Florida",
      "CA" -> "California",
      "MI" -> "Michigan",
      "TX" -> "Texas",
      "NY" -> "New York",
      "MI" -> "Michigan"
    )

    val expected = 5
    input.size should be(expected)
  }

  "Map" should "update a property with duplicate key" in {
    val input = Map(
      "FL" -> "Florida",
      "CA" -> "California",
      "MI" -> "Michigan",
      "TX" -> "Texas",
      "NY" -> "New York",
      "MI" -> "Michigan",
      "MI" -> "Meecheegun"
    )

    val expected = 5
    input.size should be(expected)
    input("MI") should be("Meecheegun")
  }

  "Map" should "report if it contains a key" in {
    val input = Map(
      "FL" -> "Florida",
      "CA" -> "California",
      "MI" -> "Michigan",
      "TX" -> "Texas",
      "NY" -> "New York",
      "MI" -> "Michigan"
    )

    input contains("MI") should be(true)
    input contains("NJ") should be(false)
  }

  "Map" should "allow keys to be added with + operation" in {
    val input = Map(
      "FL" -> "Florida",
      "CA" -> "California",
    )

    val expected = "New York"
    val next = input + ("NY" -> expected)
    next("NY") should be(expected)
  }

  "Map" should "allow keys to be removed with - operation" in {
    val input = Map(
      "FL" -> "Florida",
      "CA" -> "California",
      "MI" -> "Michigan",
      "TX" -> "Texas",
      "NY" -> "New York",
      "MI" -> "Michigan"
    )

    val expected = Map(
      "FL" -> "Florida",
      "MI" -> "Michigan",
      "TX" -> "Texas",
      "NY" -> "New York",
      "MI" -> "Michigan"
    )

    val result = input - "CA"
    result should be(expected)
  }

  "Map" should "allow removing multiple keys with - and tuple" in {
    val input = Map(
      "FL" -> "Florida",
      "CA" -> "California",
      "MI" -> "Michigan",
      "TX" -> "Texas",
      "NY" -> "New York",
      "MI" -> "Michigan"
    )

    val expected = Map(
      "FL" -> "Florida",
      "MI" -> "Michigan",
      "TX" -> "Texas",
      "MI" -> "Michigan",
    )

    val result = input - ("CA", "NY")
    result should be(expected)
  }

  "Map" should "allow multiple keys to be removed with -- operation" in {
    val input = Map(
      "FL" -> "Florida",
      "CA" -> "California",
      "MI" -> "Michigan",
      "TX" -> "Texas",
      "NY" -> "New York",
      "MI" -> "Michigan"
    )

    val expected = Map(
      "FL" -> "Florida",
      "MI" -> "Michigan",
      "TX" -> "Texas",
      "MI" -> "Michigan"
    )

    val result = input -- List("CA", "NY")
    result should be(expected)
  }

  "Map" should "infer key types to Any if not specified" in {
    val input = Map(
      "FL" -> "Florida",
      23 -> "Florida"
    )

    input contains { "FL" } should be(true)
    input contains { 23 } should be(true)
  }
}

