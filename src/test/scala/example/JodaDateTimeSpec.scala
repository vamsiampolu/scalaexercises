package example

import org.scalatest._
import com.github.nscala_time.time.Imports._

class JodaDateTimeSpec extends FlatSpec with Matchers {
  behavior of "LocalDate.parse"

  it should "accept a String and return a LocalDate instance" in {
    val input: String = "1990-12-24"
    val expected = new LocalDate(1990,12,24)
    val result = LocalDate.parse("1990-12-24")

    result should be(expected)
  }

  it should "format a LocalDate instance to a String" in {
    val input = new LocalDate(1990,12, 26)
    val expected = "1990-12-26"

    val formatter = DateTimeFormat.forPattern("YYYY-MM-dd")
    val result = formatter.print(input)

    result should be(expected)
  }
}
