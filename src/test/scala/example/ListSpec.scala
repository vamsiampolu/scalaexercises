package example

import org.scalatest._

class ListSpec extends FlatSpec with Matchers {
  behavior of "A List"

  it should "be comparable using ==" in {
    val a = List("abc","xyz")
    val b = List("abc", "xyz")
    a == b should be(true)
  }

  it should "not be comparable using equal" in {
    val a = List("abc","xyz")
    val b = List("abc", "xyz")
    a eq b should be(false)
  }

  it should "use head, headOption and tail" in {
    val a = List("a", "b", "c")
    val h = a.head
    val sh = a.headOption
    h should be("a")
    sh should be (Some("a"))

    // Mini revision
    // Option can use map and fold
    val ush = sh.map { (x:String) => x.toUpperCase }
    ush should be(Some("A"))
    val fush = ush.fold("") { (x:String) => x.toLowerCase }
    fush should be("a")

    val t = a.tail
    t should be(List("b", "c"))

    val t2 = t.tail
    t2 should be(List("c"))
  }

  it should "allow access to elements using indices" in {
    val list = List("a", "b", "c", "d", "e")
    list(0) should be("a")
    list(2) should be("c")
    list(4) should be("e")

    an [IndexOutOfBoundsException] should be thrownBy {
      println(list(5))
    }
  }

  it should "have utility methods" in {
    val a = List(1,2,3,4,5)
    val result = a.reverse
    val expected = List(5,4,3,2,1)
    result should be(expected)
    result.length should be(5)
  }

  it should "allow filter method" in {
    val a = List(1,3,5,7,9)
    val result = a filter { (num:Int) => num % 3 == 0 }

    result should be(List(3,9))
  }

  it should "have foldLeft and reduceLeft for aggregation" in {
    val a = List(1,3,5,7)
    val result = a.reduceLeft(_ + _)
    val expected = 16
    result should be(expected)

    val resultFold = a.foldLeft(0)(_ + _)
    resultFold should be(expected)
  }

  it should "be created from a Range by invoking toList" in {
    val a = 1 to 5
    val result = a.toList
    val expected = List(1,2,3,4,5)
    result should be(expected)

    val b = 1 until 5
    val result2 = b.toList
    val expected2 = List(1,2,3,4)
    result2 should be(expected2)
  }

  it should "allow prepending with the :: operator" in {
    val a = List(3)
    val b = 2 :: a
    val c = 1 :: b

    c should be(List(1,2,3))
  }

  it should "allow prepending to Nil" in {
    // Nil is treated as empty list
    val a = Nil
    val result = 1 :: a
    result should be(List(1))
  }

  it should "allow concat with ::: operator" in {
    val a = List(1)
    val b = List(2,3)

    val result = a ::: b
    val expected = List(1,2,3)

    result should be(expected)
  }

  it should "allow Nil to be concated to List" in {
    val a = List(1,2)
    val b = Nil

    val result = a ::: b
    val expected = List(1,2)

    result should be(expected)
  }

  it should "reuse their tails" in {
    val a = List(3)
    val b = 2 :: a
    val c = 1 :: b

    c.tail should be(b)
    b.tail should be(a)
    a.tail should be(Nil)
  }
}

