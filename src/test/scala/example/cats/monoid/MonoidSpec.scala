package example.cats.monoid

import org.scalatest.{FlatSpec, Matchers}

import cats.implicits._
import cats.kernel.Monoid


class MonoidSpec extends FlatSpec with Matchers {
  /**
    * Monoid extends Semigroup and adds an `empty` to it.
    * empty is an identity value.
    *
    * There is also combineAll which operates on a List of values
    *
    * We can create instances of the Monoid for types that don't have it already defined
    * (dim memories of having read about a certain newtype keyword when working with haskell
    *
    * implicit def monoidTuple[A: Monoid, B: Monoid]: Monoid[(A, B)] =
    *   new Monoid[(A, B)] {
    *     def combine(x: (A, B), y: (A, B)): (A, B) = {
    *       val (xa, xb) = x
    *       val (ya, yb) = y
    *       (Monoid[A].combine(xa, ya), Monoid[B].combine(xb, yb))
    *     }
    *     def empty: (A, B) = (Monoid[A].empty, Monoid[B].empty)
    *   }
    */

  behavior of "Monoid.empty"

  it should "be an empty string for String type" in {
    Monoid[String].empty shouldEqual ""
  }

  behavior of "Monoid#combineAll"

  it should "return the empty value when using combineAll with an empty List" in {
    Monoid[String].combineAll(List[String]()) shouldEqual Monoid[String].empty
    Monoid[Map[String, Int]].combineAll(List()) shouldEqual Monoid[Map[String, Int]].empty
  }

  it should "allow for combining more complex types" in {
    val input = List(
      Map("a" → 1, "b" → 2),
      Map("a" → 3)
    )
    val expected = Map("a" -> 4, "b" -> 2)
    Monoid[Map[String, Int]].combineAll(input) shouldEqual expected
  }

  behavior of "Foldable#foldMap"

  it should "use the available Monoid to accumulate results" in {
    val l = List(1, 2, 3, 4, 5)
    l.foldMap(identity) shouldEqual 15
    l.foldMap(_.toString) shouldEqual "12345"
  }

  it should "use the implicit monoid definition available for a Tuple2" in {
    List(1, 2, 3, 4, 5).foldMap(i ⇒ (i, i.toString)) shouldEqual (15, "12345")
  }
}
