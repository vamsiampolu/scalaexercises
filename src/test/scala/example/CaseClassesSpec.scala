package example

import org.scalatest._

case class Book(
  author:String,
  title: String,
  publisher: String
)

class OrdBook(
  author:String,
  title: String,
  publisher: String
)

case class Circle(
  x: Float,
  y: Float,
  r: Float
)

case class Name(
  first: String,
  middle: String = "",
  last: String = ""
)

class CaseClassesSpec extends FlatSpec with Matchers {

  it should "be Serializable if a case class" in {
    val bookA = Book("Roald Dahl", "Matilda", "unknown")
    bookA.isInstanceOf[Serializable] shouldBe true
  }

  it should "not be serializable if a regular class" in {
    val bookA = new OrdBook("Roald Dahl", "Matilda", "unknown")
    bookA.isInstanceOf[Serializable] shouldBe false
  }

  behavior of "equality with #== and #eq"

  it should "use structural equality with == method" in {
    val bookA = Book("Roald Dahl", "Matilda", "unknown")
    val bookB = Book("Roald Dahl", "Matilda", "unknown")

    (bookA == bookB) shouldBe true
    (bookA eq bookB) shouldBe false
  }

  behavior of "#toString"

  it should "autogenerate a toString method" in {
    val c = Circle(1.5f, 3.0f, 4.5f)
    val received = c.toString
    val expected = "Circle(1.5,3.0,4.5)"
     received should equal(expected)
  }

  behavior of "#hashcode"


  it should "create same hashcode for objects with same structure" in {
    val bookA = Book("Roald Dahl", "Matilda", "unknown")
    val bookB = Book("Roald Dahl", "Matilda", "unknown")

    (bookA.hashCode == bookB.hashCode) shouldBe true
  }

  behavior of "#copy"

  it should "duplicate all properties and create a new case class from the original" in {
    val bookA = Book("Roald Dahl", "Matilda", "unknown")
    val bookB = bookA.copy()

    bookA == bookB shouldBe true
  }

  it should "update a property if passed to copy as a a named argument" in {
    val bookA = Book("Roald Dahl", "Matilda", "unknown")
    val bookTitle = "Fantastic Mr. Fox"
    val bookB = bookA.copy(title = bookTitle)

    (bookA.author == bookB.author) shouldBe true
    bookA.title should equal("Matilda")
    bookB.title should equal(bookTitle)
  }

  behavior of "named parameters"

  it should "assign values to parameters by name" in {
    val dumbledore = Name(first = "Albus", middle = "Wulfric", last = "Dumbledore")
    dumbledore.first should equal("Albus")
    dumbledore.middle should equal("Wulfric")
    dumbledore.last should equal("Dumbledore")
  }

  it should "assign values to parameter by name regardless of order" in {
    val dumbledore = Name(middle = "Wulfric", first = "Albus",  last = "Dumbledore")
    dumbledore.first should equal("Albus")
    dumbledore.middle should equal("Wulfric")
    dumbledore.last should equal("Dumbledore")
  }

  behavior of "default parameters"

  it should "allow omitted parameters to have default value" in {
    val firstNameOnly = Name("Jane")
    firstNameOnly.first should equal("Jane")
    firstNameOnly.middle should equal("")
    firstNameOnly.last should equal("")
  }

  it should "use the parameter value if provided by the user" in {
    val jamesBond = Name("James", last = "Bond")

    jamesBond.first should equal("James")
    jamesBond.middle should equal("")
    jamesBond.last should equal("Bond")
  }

  behavior of "tupled"

  it should "accept a tuple and create a case class using the tuple" in {
    val b1 = Book.tupled(
      ("Patrick Rothfuss", "The name of the Wind", "Unknown")
    )

    b1.author should equal("Patrick Rothfuss")
    b1.title should equal("The name of the Wind")
    b1.publisher should equal("Unknown")
  }

  behavior of "unapply"

  it should "return a Option[TupleN] of the parameters" in {
    val book = Book("Roald Dahl", "Matilda", "unknown")
    Book.unapply(book) match {
      case Some((author, title, publisher)) => {
        author should equal(book.author)
        title should equal(book.title)
        publisher should equal(book.publisher)
      }
      case None => {
        None should equal(Some((book.author, book.title, book.publisher)))
      }
    }
  }
}
