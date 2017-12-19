import org.scalatest._
import java.time.LocalDate
import java.time.format.DateTimeFormatter

class Java8DateTimeSpec extends FlatSpec with Matchers {
  behavior of "LocalDate#parse"

  it should "accept a String and convert it to DateTime" in {
    val input = "1990-12-25"
    val expected = LocalDate.of(1990, 12, 25)

    val result = LocalDate.parse(input)

    result should be(expected)
  }

  behavior of "DateTimeFormatter#format"

    it should "format LocalDate to String" in {
    val formatter: DateTimeFormatter = DateTimeFormatter.ofPattern("YYYY-MM-dd")
    val input = LocalDate.of(1990,12,25)
    val expected = "1990-12-25"

    val result = formatter.format(input)

    result should be(expected)
  }

  behavior of "LocalDate#format"
  it should "format LocalDate to String" in {
    val formatter: DateTimeFormatter = DateTimeFormatter.ofPattern("YYYY-MM-dd")
    val input = LocalDate.of(1990,12,25)
    val expected = "1990-12-25"

    val result = input.format(formatter)

    result should be(expected)
  }
}