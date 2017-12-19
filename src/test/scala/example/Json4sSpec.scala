package example

import org.scalatest._
import java.time._
import java.time.format._

import org.json4s.{CustomSerializer, DefaultFormats}
import org.json4s._
import org.json4s.native.JsonMethods._
import org.json4s.native.Serialization.{read, write}
import org.json4s.JsonAST.{JString, JNull}

object LocalDateSerializer extends CustomSerializer[LocalDate](format => ({
  case JString(str) => LocalDate.parse(str)
  case JNull => null
}, {
    case value: LocalDate  => {
      val formatter = DateTimeFormatter.ofPattern("YYYY-MM-dd")
      JString(formatter.format(value))
    }
  }
))

case class Dates(createdAt: LocalDate, updatedAt: LocalDate, startDate: LocalDate, endDate: LocalDate )


class Json4sSpec extends FlatSpec with Matchers {
  behavior of "read"

  it should "read Java 8 Local Date Time from JSON String with CustomSerializer" in {

    implicit val formats =  org.json4s.DefaultFormats ++ List(LocalDateSerializer)


    val input =
      """
        |{
        |  "createdAt": "1999-12-10",
        |  "updatedAt": "1999-12-16",
        |  "startDate": "2000-01-02",
        |  "endDate": "2000-01-16"
        |}
      """.stripMargin

    val expected = Dates(
      LocalDate.of(1999,12,10),
      LocalDate.of(1999,12,16),
      LocalDate.of(2000,1,2),
      LocalDate.of(2000,1,16)
    )

    val result = read[Dates] { input }

    result should be(expected)
  }

  behavior of "write"

  it should "accept a Scala object and write to JSON using custom formatter" in {
    implicit val formats =  org.json4s.DefaultFormats ++ List(LocalDateSerializer)

    val input = Dates(
      LocalDate.of(1999,12,10),
      LocalDate.of(1999,12,16),
      LocalDate.of(2000,1,2),
      LocalDate.of(2000,1,16)
    )

    val result = write[Dates](input)
    val parsedResult = parse(result)

    val createdAt = (parsedResult \ "createdAt")
    createdAt.values should be("1999-12-10")

    val updatedAt = (parsedResult \ "updatedAt")
    updatedAt.values should be("1999-12-16")

    val startDate = (parsedResult \ "startDate")
    startDate.values should be("2000-01-02")

    val endDate = (parsedResult \ "endDate")
    endDate.values should be("2000-01-16")
  }
}
