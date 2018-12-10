package example.cats.apply

import cats.Apply
import cats.implicits._
import org.scalatest.{FlatSpec, Matchers}

class ApplySpec extends FlatSpec with Matchers {
  /*
  * Apply is a functor with the addition of a single fuction `ap`
  * class Apply where
  *   ap:: F f => (F a -> F b) -> F a -> F b
  *
  *
  * Apply also offers
  *   variations of ap{2-22} as well as apN
  *   variations of map{2-22} as well as mapN
  *   variations of tuple as well as tupleN
  * */

  val intToString: Int ⇒ String = _.toString

  val double: Int ⇒ Int = _ * 2

  val addTwo: Int ⇒ Int = _ + 2

  val plusOne = (x: Int) =>  x + 1

  val addArity2 = (a: Int, b: Int) ⇒ a + b

  val addArity3 = (a: Int, b: Int, c: Int) ⇒ a + b + c


  behavior of "map"

  it should "be the same as Functor#map" in {
    Apply[Option].map(Some(1))(intToString) should be(
      Some("1")
    )
    Apply[Option].map(Some(1))(double) should be(
      Some(2)
    )
    Apply[Option].map(None)(addTwo) should be(
      None
    )
  }

  it should "provide mapN for functions with arity of 2 or more" in {
    Apply[Option].map2(Some(1), Some(2))(addArity2) shouldEqual Some(3)

    Apply[Option].map2(Some(1), None)(addArity2) shouldEqual None

    Apply[Option].map3(Some(1), Some(2), Some(3))(addArity3) shouldEqual Some(6)
  }

  behavior of "compose"

  it should "work with ap when a function is wrapped in the context" in {
    val listOpt = Apply[List] compose Apply[Option]

    val actual = listOpt.ap(List(Some(plusOne)))(List(Some(1), None, Some(3)))

    val expected = List(Some(2), None, Some(4))

    actual shouldEqual expected
  }

  behavior of "ap"

  it should "take a function wrapped in a Functor context and invoke it on values within the same context" in {
    Apply[Option].ap(Some(intToString))(Some(1)) shouldEqual Some("1")

    Apply[Option].ap(Some(double))(Some(1)) shouldEqual Some(2)

    Apply[Option].ap(Some(double))(None) shouldEqual None

    Apply[Option].ap(None)(Some(1)) shouldEqual None

    Apply[Option].ap(None)(None) shouldEqual None
  }

  it should "provide apN for functions with arity of 2 or more" in {

    Apply[Option].ap2(Some(addArity2))(Some(1), Some(2)) shouldEqual Some(3)

    Apply[Option].ap2(Some(addArity2))(Some(1), None) shouldEqual None

    Apply[Option].ap3(Some(addArity3))(Some(1), Some(2), Some(3)) shouldEqual Some(6)

  }

  behavior of "tupleN"

  it should "provide a function to convert n Functors to a functor with their contents in a tuple" in {
    Apply[Option].tuple2(Some(1), Some(2)) shouldEqual Some((1, 2))

    Apply[Option].tuple3(Some(1), Some(2), Some(3)) shouldEqual Some((1,2,3))
  }

  behavior of "deprecated |@| syntax"

  it should "create instances of Apply which work with map" in {
    val option2 = Option(1) |@| Option(2)
    // this allows for the use of map instead of map2
    // we can pretend everything is a functor and move on
    option2 map addArity2 shouldEqual Some(3)
  }

  it should "use apWith instead of apN" in {
    val option2 = Option(1) |@| Option(2)
    option2 apWith Some(addArity2) shouldEqual Some(3)
  }

  it should "use tupled instead of tupleN" in {
    val option2 = Option(1) |@| Option(2)
    option2.tupled shouldEqual Some((1,2))
  }
}
