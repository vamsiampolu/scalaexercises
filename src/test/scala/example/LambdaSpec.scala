package example

import org.scalatest._

class LambdaSpec extends FlatSpec with Matchers {
  behavior of "Lambda"

  it should "work" in {
    def fn = (x:Int) => x + 1
    val fn_no_sugar = new Function1[Int, Int] {
      def apply(a: Int) = {
        a + 1
      }
    }
    val result = fn(1)
    result shouldBe  2
  }

  it should "use closures" in {
    val y = 2
    def fn = (x: Int) => {
      x + y
    }
    val result = fn(4)
    result shouldBe 6
  }

  it should  "accept functions" in {
    val x = 2
    val fn = (y: Int) => { x + y + 1}
    val fn2 = (a: Int, closure:Int => Int) => {
      closure(a)
    }
    val result = fn2(3, fn)
    result shouldEqual 6
  }

  // without syntax sugar
  it should "return functions" in {
    val x = 2
    val fn = (y: Int) => {
      // return value has been desugared
      new Function1[Int, Int] {
        def apply(z:Int) = {
          x + y + z
        }
      }
    }
    val fn2 = fn(3)
    val result = fn2(1)
    result should be(6)
  }

  // with syntax sugar
  it should "return functions2" in {
    val x = 2
    val fn = (y:Int) => { (z:Int) => x + y + z }
    val fnb = fn(3)
    val result = fnb(1)
    result should be(6)
  }

  it should "work with isInstanceOf" in {
    val fn = (y:Int) => y + 1
    fn.isInstanceOf[Function1[Int, Int]] should be(true)
  }

  it should "work with shouldBe a " in {
    val fn = (y:Int) => y + 1
    fn shouldBe a[Function1[_, _]]
  }

  it should "be passed to map" in {
    def makeUpper: List[String] => List[String] = xs => xs map {
      _.toUpperCase
    }

    val a = List("abc", "xyz", "123")
    val b = makeUpper(a)
    val expectedB = List("ABC", "XYZ", "123")

    b should be(expectedB)

    def makeWhateverYouLike(xs: List[String], sideEffect: String => String) = {
      xs map sideEffect
    }

    val c = makeWhateverYouLike(b, { x => x.toLowerCase })
    c should be(a)

    val d = List("John", "Mark")
    def myName(name:String) = s"My name is $name"
    val expectedE = List("My name is John", "My name is Mark")
    val e = makeWhateverYouLike(d, myName)

    List("Scala", "Erlang", "Clojure") map (_.length) should be(
      List(5,6,7)
    )
  }
}

