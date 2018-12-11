package example.cats.semigroup

import cats.implicits._
import cats.kernel.Semigroup
import org.scalatest.{FlatSpec, Matchers}

class SemigroupSpec extends FlatSpec with Matchers {

  /**
  * A Semigroup has an associative `combine` operation
    *
    * For
    *  Int -> addition
    *  List -> concat
    *  Option[T] -> if both have values, then the combine of T is used
    *            -> if one of the values is None, the other value is returned
    *
    *  Map -> ++
    *  Semigroup also has an apply which returns the implicit.
    *
    *  Function1 -> return a Function which is a combination of the function.
    *  Replace the missing hole with `apply` and the results are combined together.
    *
    *  The combine operator can be used to combine values within recursively??
    *
  * */

  behavior of "Semigroup.combine"

  it should "use addition as the default operation for Int" in {
    Semigroup[Int].combine(1, 2) shouldEqual 3
  }


  it should "use concatenation as the default operation for List" in {
    Semigroup[List[Int]].combine(List(1,2,3), List(4,5,6)) shouldEqual List(1,2,3,4,5,6)
  }

  it should "use the nested type's combine as part of the default operation for Option" in {
    Semigroup[Option[Int]].combine(Some(1), Some(2)) shouldEqual Some(3)
  }

  it should "return the other value when one of the values is None" in {
    Semigroup[Option[Int]].combine(Some(2), None) shouldEqual Some(2)
  }

  it should "apply the value to the missing param on a Function and then perform combine" in {
    Semigroup[Int => Int].combine(_ + 1, _ * 10).apply(6) shouldEqual 67
  }

  it should "compose well and combine subtypes recursively ???" in {
    val inputA = Map[String, Map[String, Int]](
      "foo" -> Map("bar" -> 5)
    )

    val inputB = Map[String, Map[String, Int]](
      "foo" -> Map("bar" -> 6),
      "baz" -> Map()
    )

    val expected = Map[String, Map[String, Int]](
      "foo" -> Map("bar" -> 11),
      "baz" -> Map()
    )

    (inputA combine inputB) shouldEqual expected
  }

  behavior of "|+| scalaz syntax"

  it should "combine values for which the typeclass is defined" in {
    // NOTE: cannot use `Some` and `None`, they have no clue about `Semigroup`.
    // need to explicitly use Option() value constructor.

    val none:Option[Int] = None
    Option(1) |+| Option(2) shouldEqual Option(3)
    none |+| Option(2) shouldEqual Option(2)
    none |+| none shouldEqual none
    Option(2) |+| none shouldEqual Option(2)
  }
}
