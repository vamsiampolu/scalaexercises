package example

import org.scalatest._
import com.softwaremill.sttp._
import com.softwaremill.sttp.json4s._

case class HttpBinResponse(
  args: Map[String, String],
  origin: String,
  headers: Map[String,String]
)

class SttpSpec extends FlatSpec with Matchers {
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

  it should "send a GET request parse response as JSON" in {
    implicit val backend = HttpURLConnectionBackend()


    val queryParams = Map("foo" -> "bar", "bugs" -> "life")

    val endpoint:Uri = uri"http://httpbin.org/get?foo=bar"

    val request = sttp
      .get(endpoint)
      .response(asJson[HttpBinResponse])

    val response = request.send()
    // response.body is an Either

    response.code should be(200)
    val res = response.body.fold(_ => { "Error" }, a => { a })

    res shouldBe a[HttpBinResponse]
    println(res.origin)
  }
}
