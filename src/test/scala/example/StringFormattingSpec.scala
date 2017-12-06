package example

import org.scalatest._

class StringFormatting extends FlatSpec with Matchers {
  behavior of "%s"

  it should "allow for string interpolation" in {
    val app = "Application"
    val expected = "Hello, World Application"

    val result = "Hello, World %s".format(app)

    result should be(expected)
  }

  behavior of "%c"

  it should "accept a char and convert it to a string" in {
    val expected = "a"
    val result = "%c".format('a')

    result should be(expected)
  }

  it should "accept unicode characters" in {
    val expected = "♕"
    val result = "%c".format('♕')
    result should be(expected)
  }

  it should "accept octal characters" in {
    val input = '\141'
    val expected = "a"

    val result = "%c".format(input)

    result should be(expected)
  }

  it should "work with escape sequences" in {
    "%c".format('\"') should be("\"")
    "%c".format('\\') should be("\\")
  }

  behavior of "%d"

  it should "accept numbers and interpolate them inside the string" in {
    val expected = "7 samurai"
    val j = 12
    val result = "%d samurai" format (j - 5)
    result should be(expected)
  }

  "formatting" should "allow multiple items" in {
    val j = 190
    val k = "vodka"

    val expected = "90 bottles of vodka on the wall"

    val result = "%d bottles of %s on the wall" format(j - 100, k)

    result should be(expected)
  }
}
