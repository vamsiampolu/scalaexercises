package example.cats.validated

import org.scalatest.{FlatSpec, Matchers}

import cats.implicits._
import cats.Apply

import cats.data.{NonEmptyList, Validated, ValidatedNel}
import cats.data.Validated.{Invalid, Valid}

import example.cats.validated.ConfigError._


case class Address(
                    houseNumber: Int,
                    street: String
                  )
case class Person(
                   name: String,
                   age: Int,
                   address: Address
                 )

class ValidatedSpec extends FlatSpec with Matchers {

  behavior of "ParallelValidate"

  /**
    * Validated is useful if we want to report multiple errors at the same time.
    * This is suited for situations where multiple bits of data is validated independently
    *
    * Validated is an type constructor with two value constructors.
    *
    *   - Valid
    *
    *   - Invalid
    *
    *   The example uses a `Read` parser to read configuration into the `Config` model
    *   and a `ConfigError` to represent possible failures.
    *
    *   We created a ParallelValidate to Validate Config using ValidatedNel[ConfigError, TypeOfItem]
    */

  import ParallelValidate._

  it should "be valid when correct data is provided" in {
    val model = Config(Map(("url", "127.0.0.1"), ("port", "1337")))

    val valid = parallelValidate(
      model.parse[String]("url").toValidatedNel,
      model.parse[Int]("port").toValidatedNel
    )(ConnectionParams.apply)

    valid.isValid shouldBe true
    valid.getOrElse(ConnectionParams("", 0)) should be(ConnectionParams("127.0.0.1", 1337))
  }

  it should "be Invalid and combine errors into a NonEmptyList" in {
    val input = Config(
      Map(
        ("endpoint", "127.0.0.1"),
        ("port", "not a number")
      )
    )

    val expected = Validated.invalid(
      NonEmptyList(
        MissingConfig("url"),
        List(ParseError("port"))
      )
    )

    val subject = parallelValidate(
      input.parse[String]("url").toValidatedNel,
      input.parse[Int]("port").toValidatedNel
    )(ConnectionParams.apply)

    subject.isInvalid shouldBe true
    subject should equal(expected)
  }

  it should "allow us to use map4 as an Applicative instead of relying on ParallelValidate" in {
    val config = Config(Map(
        ("name", "cat"),
        ("age", "not a number"),
        ("houseNumber", "1234"),
        ("lane", "feline street")
      )
    )

    val expected = Invalid(NonEmptyList[ConfigError](
        MissingConfig("street"),
        List(MissingConfig("house_number"),
          ParseError("age")
        )
      )
    )

    val personFromConfig: ValidatedNel[ConfigError, Person] = Apply[ValidatedNel[ConfigError, ?]].map4(
      config.parse[String]("name").toValidatedNel,
      config.parse[Int]("age").toValidatedNel,
      config.parse[Int]("house_number").toValidatedNel,
      config.parse[String]("street").toValidatedNel
    ) {
      case (name, age, houseNumber, street) => Person(
        name,
        age,
        Address(
          houseNumber,
          street
        )
      )
    }

    personFromConfig.isInvalid shouldBe true
    personFromConfig shouldEqual expected
  }

  behavior of "Validated#andThen"

  it should "pass valid value to the anonymous function to return a Validated instance" in {
    val config = Config(Map(
      ("name", "cat"),
      ("age", "not a number"),
      ("house_number", "1234"),
      ("lane", "feline street")
    )
    )
    val houseNumber = config.parse[Int]("house_number") andThen{n =>
      if (n > 0) {
        Validated.valid(n)
      } else {
        Validated.invalid(ParseError("house_number"))
      }
    }

    houseNumber.isValid shouldBe true
    houseNumber shouldEqual Validated.valid(1234)
  }

  it should "pass Valid value for rejection after further validation" in {
    val config = Config(Map("house_number" → "-42"))

    val houseNumber = config.parse[Int]("house_number").andThen { n ⇒
      if (n >= 0) Validated.valid(n)
      else Validated.invalid(ParseError("house_number"))
    }

    houseNumber.isValid shouldBe false
    houseNumber shouldEqual Validated.invalid(ParseError("house_number"))
  }

  // NOTE: Discrepency between expected behavior based on scalaexercises and actual behavior
  it should "use withEither to fail on first error(???)" in {
    val config = Config(Map(("house_number","-42")))
    val houseNumber = config.parse[Int]("house_number")

    val actual = houseNumber.withEither {eitherValue =>
      eitherValue.flatMap{i =>
        if (i >= 0) Either.right(i)
        else Either.left(ParseError("house_number"))
      }
    }

    houseNumber.isValid shouldBe true
    // houseNumber shouldEqual Validated.invalid(ParseError("house_number"))
    houseNumber shouldEqual Validated.valid(-42)
  }

  it should "use mapN to convert a set of Validated to a result" in {
    val config = Config(
      Map(
        ("name", "cat"),
        ("age", "not a number"),
        ("houseNumber", "1234"),
        ("lane", "feline street")
      )
    )

    val personFromConfig = (
      config.parse[String]("name").toValidatedNel,
      config.parse[Int]("age").toValidatedNel,
      config.parse[Int]("house_number").toValidatedNel,
      config.parse[String]("street").toValidatedNel
    ).mapN{
      case (name, age, houseNumber, street) => Person(name, age, Address(houseNumber, street))
    }

    val expected = Invalid(NonEmptyList[ConfigError](
      MissingConfig("street"),
      List(MissingConfig("house_number"),
        ParseError("age")
        )
      )
    )

    personFromConfig.isInvalid shouldBe true
    personFromConfig shouldEqual expected
  }
}
