package example.cats.applicative

import cats.implicits._
import cats.{Applicative, Monad}
import org.scalatest.{FlatSpec, Matchers}

class ApplicativeSpec extends FlatSpec with Matchers {
  /**
    * An `Applicative` extends `Apply` with a single method called `pure`
    * which takes a value and lifts it into the context of a functor
    *
    * (???) Applicative is a generalization of a monad, allowing expression of effectual
    * computation in a pure functional way.
    *
    * Applicative is preferred when the structure of the computation is known.
    * This makes it possible to perform static analysis on the Applicative value.
    */

  behavior of "Applicative.pure"

  it should "lift a value into a functor" in {
    Applicative[Option].pure(1) shouldEqual Some(1)
    Applicative[List].pure(1) shouldEqual List(1)
  }

  it should "work with a composed Functor" in {
    val instance = Applicative[List] compose Applicative[Option]
    instance.pure(1) shouldEqual List(Some(1))
  }

  it should "not be so confusing and wierd that I cannot write a spec" in {
    Monad[Option].pure(1) shouldEqual Some(1)
    Applicative[Option].pure(1) shouldEqual Some(1)
  }
}
