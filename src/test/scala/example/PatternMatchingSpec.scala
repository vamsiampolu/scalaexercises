package example

import org.scalatest._

object goldilocks {
  def bears1(expr: Any): String = {
    expr match {
      case ("porridge", bear) ⇒ bear + " said someone's been eating my porridge"
      case ("chair", bear) ⇒ bear + " said someone's been sitting in my chair"
      case ("bed", bear) ⇒ bear + " said someone's been sleeping in my bed"
      case _ ⇒ "what?"
    }
  }
}

class PatternMatchingSpec extends FlatSpec with Matchers {
  behavior of "Pattern Matching"

  it should "match primitives to returning primitive values" in {
     val expected = 2
     val input = "blue"

     val result = input match {
       case "red" => { 1 }
       case "blue" => { 2 }
       case "green" => { 3 }
     }
     result should be(expected)
  }

  it should "match primitives to return complex values" in {
    val red = Map(
      "red" -> "255",
      "green" -> 0,
      "blue" -> 0
    )

    val green = Map(
      "red" -> 0,
      "green" -> "255",
      "blue" -> 0
    )

    val blue = Map(
      "red" -> 0,
      "blue" -> "255",
      "green" -> 0
    )

    val input = "blue"

    val result = input match {
      case "red" => red
      case "blue" => blue
      case "green" => green
    }

    result should be(blue)
  }

  it should "fall through to default case" in {
    val input = "orange"
    val expected = 4

    val result = input match {
      case "red" => 1
      case "blue" => 2
      case "green" => 3
      case _ => 4
    }

    result should be(expected)
  }

  it should "match complex expressions" in {
    val input = ("Mama", "brownies")
    val expected = "Mama eating brownies"

    val result = input match {
      case ("Papa", "porridge") => "Papa eating porridge"
      case ("Mama", "brownies") => expected
      case _ => "Unknown diner in the pantry"
    }

    result should be(expected)
  }

  it should "match wildcard expressions" in {
    val input = ("Mama", "brownies")
    val expected = "Mama eating breakfast"

    val result = input match {
      case ("Papa", _) => "Papa eating breakfast"
      case ("Mama", _) => expected
      case _ => "Unknown diner in the pantry"
    }

    result should be(expected)
  }

  it should "use parts of the expression in the result" in {
    val resPapa = goldilocks.bears1(("porridge", "papa"))
    val expectedPapa = "papa said someone's been eating my porridge"
    resPapa should be(expectedPapa)

    val resPapa2 = goldilocks.bears1(("chair", "papa"))
    val expectedPapa2 = "papa said someone's been sitting in my chair"

    expectedPapa2 should be(resPapa2)

    val res3 = goldilocks.bears1(("unknown", "unknown"))
    res3 should be("what?")
  }

  it should "use backquotes can be used to refer to existing variables" in {
    info("The values enclosed in backticks could be referring to function parameters")
    val porridge = "porridge"
    val brownies = "brownies"
    val chips = "chips"

    val papa = "papa"
    val mama = "mama"
    val baby = "baby"

    val input = (porridge, papa)

    val result = input match {
      case (`porridge`, `papa`) => 1
      case (`porridge`, `mama`) => 2
      case (`porridge`, `baby`) => 3
      case (`chips`, `baby`) => 4
      case (`brownies`, `baby`) => 4
      case _ => 0
    }

    result should be(1)
  }

  it should "match on List(s)" in {
    val expected = 2
    val input = List(1, expected, 3)

    val result = input match {
      case x :: xs => { xs head }
      case _ => 0
    }

    result should be(expected)
  }

  it should "match on List with 2 or more elements only" in {
    val expected = 2
    val badExpected = 0

    val input = List(1, expected, 3)
    val badInput = List(5)

    val result = input match {
      case _ :: y :: _ => y
      case _ => 0
    }

    val badResult = badInput match {
      case _ :: y :: _ => y
      case _ => 0
    }

    result should be(expected)
    badResult should be(badExpected)
  }

  it should "match List with 2 elements only" in {
    val expected = 2
    val badExpected = 0

    val input = List(1, expected)
    val badInput = List(5, 5, 5, 5)

    val result = input match {
      case _ :: y :: _ => y
      case _ => 0
    }

    val badResult = badInput match {
      case _ :: y :: Nil => y
      case _ => 0
    }

    result should be(expected)
    badResult should be(badExpected)
  }
}
