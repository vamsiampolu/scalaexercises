package example

import scala.sys.error
import org.scalatest._

case class HttpBinResponse(
  args: Map[String, String],
  origin: String,
)

object HttpBinResponse {
  import io.circe._
  implicit val encoder: Encoder[HttpBinResponse] =
      Encoder.forProduct2("args", "origin")(o => (o.args, o.origin))
  implicit val decoder: Decoder[HttpBinResponse] =
      Decoder.forProduct2("args", "origin")(HttpBinResponse.apply)
}

class SttpSpec extends FlatSpec with Matchers with EitherValues {
  import com.softwaremill.sttp._
  behavior of "Sttp using HttpUrlConnection Backend"

  it should "send a GET request and recieve a response string" in {
    implicit val backend = HttpURLConnectionBackend()

    val endpoint:Uri = uri"http://httpbin.org/get"

    val request = sttp
      .get(endpoint)

    val response = request.send()
    // response.body is an Either
    val str = response.body.fold(_ => {
      "Error"
    }, a => {
      a
    })
    response.code should be(200)

    str shouldBe a[String]
    str should include("origin")
    str should include("args")
    str should include("headers")
  }

  it should "send a GET request parse response as JSON using json4s" in {
    import com.softwaremill.sttp.json4s._

    implicit val backend = HttpURLConnectionBackend()


    val queryParams = Map("foo" -> "bar", "bugs" -> "life")

    val endpoint:Uri = uri"http://httpbin.org/get?$queryParams"

    val request = sttp
      .get(endpoint)
      .response(asJson[HttpBinResponse])

    val response = request.send()
    // response.body is an Either

    response.code should be(200)
    val res = response.body.fold(
      _ => { error("Error") },
      a => { a }
    )

    res shouldBe a[HttpBinResponse]
    res match {
      case HttpBinResponse(_,origin) => {
        println("-----------------------------")
        print("The origin for the request is ")
        print(origin)
        println("-----------------------------")
      }
      case _ => "Error"
    }
    res.args should contain("foo" -> "bar")

    // assert for a key or value independently if required
    res.args should contain key "bugs"
    res.args should contain value "life"
    res.origin.length should be >10

    // Look ma, EitherValues can extract values from Either automatically
    // without having to write a lot of boilerplate
    val rightValue = response.body.right.value
    rightValue shouldBe a[HttpBinResponse]
    rightValue.args should contain("foo" -> "bar")
    rightValue.args should contain key "bugs"
    rightValue.args should contain value "life"
    rightValue.origin.length should be >10
  }

   it should "send a GET request parse response as JSON using circe" in {
    import com.softwaremill.sttp.circe._

    implicit val backend = HttpURLConnectionBackend()

    val queryParams = Map("foo" -> "bar", "bugs" -> "life")

    val endpoint:Uri = uri"http://httpbin.org/get?$queryParams"

    val request = sttp
      .get(endpoint)
      .response(asJson[HttpBinResponse])

      val response = request.send()
      // response.body is an Either

      response.code should be(200)

      val rightValue = response.body.flatMap(a => { a }).right.value
      rightValue shouldBe a[HttpBinResponse]
      rightValue.args should contain("foo" -> "bar")
      rightValue.args should contain key "bugs"
      rightValue.args should contain value "life"
      rightValue.origin.length should be >10
  }
}
