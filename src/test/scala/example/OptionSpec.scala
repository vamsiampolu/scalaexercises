package example

import org.scalatest._

object OptionExample {
  def mayHaveValue: Boolean => Option[String] = flag => if(flag) Some("Found value") else None

  def mayHave3: Boolean => Option[Int] = flag => if(flag) Some(3) else None
}

class OptionSpec extends FlatSpec with Matchers with OptionValues {
  behavior of "Option"

  it should "have Some('value')" in {
    val someValue: Option[String] = Some("value")
    someValue should be(Some("value"))

    // OptionValues adds a .value
    someValue.value should be("value")
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

  behavior of "Pattern Matching"

  it should "pattern matching be FoundValue" in {
    val input = OptionExample.mayHaveValue(true)
    // simpler assertion
    input.value should be("Found value")
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

  behavior of "Higher Order Functions"

  it should "work with map" in {
    val happy_input = OptionExample.mayHave3(true)
    val sad_input = OptionExample.mayHave3(false)

    val happy = happy_input map { _ * 1.5 }

    happy.value should be(4.5)
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
}
