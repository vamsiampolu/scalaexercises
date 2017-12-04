package example

import org.scalatest._

class MapSpec extends WordSpec with Matchers {

  "A Map" when {
    "initialized " should {
      "use shorthand notation" in {
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

      "allow access to a value with apply method when the key is defined" in {
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

      }

      "throw an exception when using apply if element is not found" in {
        val input = Map(
          "FL" -> "Florida",
          "CA" -> "California",
          "MI" -> "Michigan",
          "TX" -> "Texas",
          )

        a [NoSuchElementException] should be thrownBy {
          input apply { "WI" }
        }
      }

      "allow values to be accessed directly" in {
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
      }

      "throw exception when values are accessed directly for a key that does not exist" in {
        val input = Map(
          "FL" -> "Florida",
          "CA" -> "California",
          "MI" -> "Michigan",
          "TX" -> "Texas",
          )

        // it will throw an exception if you lookup non-existant key
        a [NoSuchElementException] should be thrownBy {
          input("WI")
        }
      }

      "safely retrieve value to Option[_] when using get method" in {
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
      }

      "retrieve nothing when accessing non-existing key using get method" in {
        val input = Map(
          "FL" -> "Florida",
          "CA" -> "California",
          "MI" -> "Michigan",
          "TX" -> "Texas",
          )

        val result = input get("WI")
        result should be(None)
      }

      "return corresponding value if a key exists using getOrElse" in {
        val expected = "New York"
        val defaultValue = "missing value" 
        val input = Map(
          "FL" -> "Florida",
          "CA" -> "California",
          "MI" -> "Michigan",
          "TX" -> "Texas",
          "NY" -> expected
        )

        val result = input getOrElse("NY", defaultValue)
        result should be(expected)
      }

      "provide a default if particular key does not exist using getOrElse" in {
        val expected = "missing value" 
        val input = Map(
          "FL" -> "Florida",
          "CA" -> "California",
          "MI" -> "Michigan",
          "TX" -> "Texas",
          "NY" -> expected
        )

        val result = input getOrElse("WI", expected)
        result should be(expected)
      }

      "allow one fallback value when accessing a key that does not exist when defining the Map using withDefaultValue" in {
        val expected = "Puerto Rico"
        val input = Map(
          "FL" -> "Florida",
          "CA" -> "California",
          "MI" -> "Michigan",
          "TX" -> "Texas",
          "NY" -> "New York"
        ) withDefaultValue expected

        input("WI") should be(expected)
        input apply { "WI" } should be(expected)
        input get("WI") should be(None)
      }

      "allow accessing its size by using the size method" in {
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

      "take the latest value in case a key is defined twice" in {
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

      "check for the existence of a key using contains method" in {
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

      "add keys with the + operator" in {
        val input = Map(
          "FL" -> "Florida",
          "CA" -> "California",
          )

        val expected = "New York"
        val next = input + ("NY" -> expected)
        next("NY") should be(expected)
      }

      "remove can remove individual key with - operator" in {
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

      "remove multiple keys using - operator" in {
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

      "remove a list of keys using -- operator" in {
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

      "allow concatenation using ++ operator" in {
        val a = Map("a" -> "A") 
        val b = Map("b" -> "B")
        val expected = Map("a" -> "A", "b" -> "B")

        val result = a ++ b
        result should be(expected)
      }

      "provide a list of all values when values is invoked on it" in {
        val input = Map(
          "FL" -> "Florida",
          "CA" -> "California",
          "MI" -> "Michigan",
          "TX" -> "Texas",
          "NY" -> "New York"
        )


        // ordering of values is not guaranteed
        input.values should contain("Florida")
        input.values should contain("Michigan")
        input.values should contain("New York")
      }
    }

    "provide a List of all keys when keys is invoked" in {
      val input = Map(
        "FL" -> "Florida",
        "CA" -> "California",
        "MI" -> "Michigan",
        "TX" -> "Texas",
        "NY" -> "New York"
      )

      val result = input.keys

      // ordering of keys is not guaranteed
      result should contain("FL")
      result should contain("MI")
      result should contain("NY")
    }

    "provide a Set of keys when using keySet method" in {
      val input = Map(
        "FL" -> "Florida",
        "CA" -> "California",
        "MI" -> "Michigan",
        "TX" -> "Texas",
        "NY" -> "New York"
      )
      val expected = Set("FL", "CA", "MI", "TX", "NY")

      val result = input.keySet

      result should be(expected)
    }

    "be deeply equal to another map with the same keys and values" in {
      val myMap1 =
        Map("MI" → "Michigan", "OH" → "Ohio", "WI" → "Wisconsin", "IA" → "Iowa")
      val myMap2 =
        Map("WI" → "Wisconsin", "MI" → "Michigan", "IA" → "Iowa", "OH" → "Ohio")

      myMap1.equals(myMap2) should be(
        true
      )
    }
  }
}

