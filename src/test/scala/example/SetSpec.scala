package example

import org.scalatest._

class SetSpec extends FlatSpec with Matchers {
  behavior of "A Set"

  it should "contain distinct values" in {
    val result = Set("AB", "CD", "AB")
    val expected = Set("AB", "CD")
    result should be(expected)
  }

  it should "tell us its size" in {
    val input = Set("New York", "New Jersey", "Hawaii")
    val expected = 3

    val result = input.size
    result should be { expected }
  }

  it should "allow adding of elements using +" in {
    val input = Set("Michigan", "Ohio", "Wisconsin", "Iowa")
    val expected = Set("Michigan", "Ohio", "Wisconsin", "Iowa", "Illinois")

    val result = input + "Illinois"

    result should be(expected)
  }

  it should "allow for use a contains function" in {
    val input = Set("Michigan", "Ohio", "Wisconsin", "Iowa")

    input("Michigan") shouldBe true
    input("New York") shouldBe false
  }

  it should "allow for checking for member" in {
    val input = Set("Michigan", "Ohio", "Wisconsin", "Iowa")
    val t_cond = input.contains { "Michigan" }
    val f_cond = input.contains("New York")
    t_cond should be(true)
    f_cond shouldBe false
  }

  it should "allow removing one element with -" in {
    val input = Set("Michigan", "Ohio", "Wisconsin", "Iowa")
    val expected = Set("Michigan", "Wisconsin", "Iowa")

    val result = input - "Ohio"

    result.contains("Ohio") shouldBe false
    result.size shouldBe 3
    result should be(expected)
  }

  it should "allow removing multiple elements with -" in {
    val input = Set("Michigan", "Ohio", "Wisconsin", "Iowa")
    val expected = Set("Wisconsin", "Iowa")

    val result = input - ("Ohio", "Michigan")

    result.contains("Ohio") shouldBe false
    result.contains("Michigan") shouldBe false
    result.size shouldBe 2
    result should be(expected)
  }

  it should "allow removing multiple elements using --" in {
    val input = Set("Michigan", "Ohio", "Wisconsin", "Iowa")
    val expected = Set("Wisconsin", "Iowa")

    val result = input -- List("Ohio", "Michigan")

    result.contains("Ohio") shouldBe false
    result.contains("Michigan") shouldBe false
    result.size shouldBe 2
    result should be(expected)
  }

  it should "not throw error when removing non-existant elements" in {
    val input = Set("Michigan", "Ohio", "Wisconsin", "Iowa")

    val result = input - "New York"

    result should be(input)
  }

  it should "support intersect operation" in {
    val a = Set("Michigan", "Ohio", "Wisconsin", "Iowa")
    val b = Set("Michigan", "New York", "New Jersey", "Wisconsin")

    val expected = Set("Michigan", "Wisconsin")
    val result = a intersect b

    result should be(expected)
  }

  it should "support intersect operation using & operator" in {
    val a = Set("Michigan", "Ohio", "Wisconsin", "Iowa")
    val b = Set("Michigan", "New York", "New Jersey", "Wisconsin")

    val expected = Set("Michigan", "Wisconsin")
    val result = a & b

    result should be(expected)
  }

  it should "support union operation" in {
    val a = Set("Michigan", "Ohio", "Wisconsin", "Iowa")
    val b = Set("Michigan", "New York", "New Jersey", "Wisconsin")

    val expected = Set("Michigan","Ohio", "Iowa", "Wisconsin", "New York", "New Jersey")
    val result = a union b

    result should be(expected)
  }

  it should "support union operation using | operator" in {
    val a = Set("Michigan", "Ohio", "Wisconsin", "Iowa")
    val b = Set("Michigan", "New York", "New Jersey", "Wisconsin")

    val expected = Set("Michigan","Ohio", "Iowa", "Wisconsin", "New York", "New Jersey")
    val result = a | b

    result should be(expected)
  }

  it should "allow checking if another Set is a subsetOf current Set" in {
    val a = Set("Michigan", "Wisconsin", "Ohio", "Iowa")
    val b = Set("Michigan", "Wisconsin", "Minnesota")
    val c = Set("Michigan", "Wisconsin")

    b subsetOf a shouldBe false
    c subsetOf a shouldBe true
  }

  it should "allow computing difference between 2 Sets" in {
    val a = Set("Michigan", "Wisconsin", "Ohio", "Iowa")
    val b = Set("Michigan", "Wisconsin")
    val expected = Set("Ohio", "Iowa")

    val result = a diff b
    result should be(expected)
  }

  it should "allow computing difference between 2 Sets using shorthand operator &~" in {
    val a = Set("Michigan", "Wisconsin", "Ohio", "Iowa")
    val b = Set("Michigan", "Wisconsin")
    val expected = Set("Ohio", "Iowa")

    val result = a &~ b
    result should be(expected)
  }
}
