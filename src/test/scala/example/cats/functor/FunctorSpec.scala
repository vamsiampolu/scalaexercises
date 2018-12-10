package example.cats.functor

import cats.implicits._
import cats.Functor
import org.scalatest.{FlatSpec, Matchers}

class FunctorSpec extends FlatSpec with Matchers {
  // Functor is a well known type class which takes types that have only one type hole

  // map is the only function defined by the category
  // all the others are derived from map ???
  /*
  * class Functor F
  *   map :: F f => (a -> b) -> f a -> f b
  *
  *   lift :: (a -> b) -> (f a -> f b)
  *   productF :: ???
  *
  *
  * */

  behavior of "map"

  it should "be used as a static method" in {
    Functor[Option].map(Option("Hello"))(_.length) should be(
      Some(5)
    )
    Functor[Option].map(None: Option[String])(_.length) should be(
      None
    )
  }

  behavior of "lift"

  it should "take a function over primitives and convert it to  a function over Functors" in {
    val lenOption: Option[String] ⇒ Option[Int] = Functor[Option].lift(_.length)
    lenOption(Some("Hello")) shouldEqual Some(5)
  }

  behavior of "fproduct"

  it should "return pairs of input and the result" in {
    val source = List("Cats", "is", "awesome")
    val product = source.fproduct(_.length).toMap

    product.get("Cats").getOrElse(0) shouldEqual 4
    product.get("is").getOrElse(0) shouldEqual 2
    product.get("awesome").getOrElse(0) shouldEqual 7
  }

  behavior of "compose"

  it should "create accept a Functor F and a Functor G and create a Functor F[G[_]]" in {
    val listOpt = Functor[List] compose Functor[Option]
    val actual = listOpt.map(List(Some(1), None, Some(3)))(_ + 1)
    val expected = List(Some(2), None, Some(4))
    actual shouldEqual expected
  }
}
