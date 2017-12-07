package example

import scala.xml._
import org.scalatest._

/* Reading List:
 * https://alvinalexander.com/scala/xml-parsing-xpath-extract-xml-tag-attributes
 * https://alvinalexander.com/scala/how-to-extract-data-from-xml-nodes-in-scala
*/

class XmlSpec extends FlatSpec with Matchers with BeforeAndAfter with OptionValues {

  var foo: Elem = _

  before {
    foo = <foo>
      <bar type="greet" style="pleasent" target="anyone">hi</bar>
      <bar type="count">1</bar>
      <bar type="color">yellow</bar>
      <baz>
        <bar type="bad">Bad</bar>
      </baz>
    </foo>
  }

  behavior of "scala.xml"

  it should "convert XML literals to scala.xml.Elem" in {
    foo shouldBe a[scala.xml.Elem]
  }

  // all the text without any nodes
  it should "allow for retrieving text from an element" in {
    foo.text should include("hi")
    foo.text should include("1")
    foo.text should include("yellow")
  }

  behavior of "projections"

  it should "use \\ to retrieve direct children elements of type" in {
    val expected = NodeSeq.fromSeq(
      <bar type="greet" style="pleasent" target="anyone">hi</bar>
      <bar type="count">1</bar>
      <bar type="color">yellow</bar>
    )
    val result = foo \ "bar"
    result should be(expected)
  }

  it should "use \\\\ to retrieve descendents of type" in {
    val expected = NodeSeq.fromSeq(
      <bar type="greet" style="pleasent" target="anyone">hi</bar>
    <bar type="count">1</bar>
    <bar type="color">yellow</bar>
    <bar type="bad">Bad</bar>
  )
    val result = foo \\ "bar"
    result should be(expected)
  }

  it should "use @<attributename> to retrieve attributeswith \\" in {
    val expected = List("greet", "count", "color")

    val result = (foo \ "bar")
      .map(x => { x \ "@type"})
      .map(x => { x.text })

    result should be(expected)
  }

  it should "query for attribute in the same way using \\" in {
    val expected = List(
      ("greet", "hi"),
      ("count", "1"),
      ("color", "yellow")
    )
    val result = (foo \ "bar" ).map(x => {(
      (x \ "@type").text,
      x.text)
    })
  }

  it should "query all attributes and return a List of values when using \\\\" in {
    val expected = List("greet", "count", "color", "bad")
    val result = (foo \\ "@type").map(x => { x.text })
  }

  behavior of "label"

  it should "retrieve label of element" in {
    foo.label should be("foo")
  }

  behavior of "attribute"

  it should "use attribute to retrieve a single attribute on a node" in {
    val node = (foo \ "bar" )(0)
    val expected = "greet"

    val result = node.attribute("type")

    result.value shouldBe a[NodeSeq]
    result.value.text should be(expected)
  }

  behavior of "attributes"

  it should "use attributes to retrieve all attributes on a node" in {
    val node = (foo \ "bar")(0)
    val expected = List(
      ("type","greet"),
      ("style","pleasent"),
      ("target","anyone")
    )

    // scala.xml.MetaData
    val attrs = node.attributes

    val result = attrs.map(x => {
      (x.key, x.value.text)
    })
    result should be(expected)
  }

  it should "retrieve value of a single attribute using unsafe access" in {
    val node = (foo \ "bar")(0)
    val attrs = node.attributes
    val goodValue = attrs("style")

    // does not throw when a non-existing key is provided,
    // instead just returns null
    goodValue.text should be("pleasent")
    attrs("unexpected") should be(null)
  }

  it should "retrieve value using Option[_] using get method" in {
    val node = (foo \ "bar")(0)
    val attrs = node.attributes

    val result = attrs.get("target")

    result.value.text should be("anyone")
    attrs.get("unexpected") should be(None)
  }

  //  ad-hoc conversion to Map, not recommended
  it should "allow for the use of foldLeft" in {
    val node = (foo \ "bar")(0)
    val expected = Map(
      "type" -> "greet",
      "style" -> "pleasent",
      "target" ->"anyone"
    )

    // scala.xml.MetaData
    val attrs = node.attributes

    val result = attrs.foldLeft(Map[String,String]())((seed,x) => {
      seed + (x.key -> x.value.text)
    })

    // it is recommended that we omit nested types when specifying isInstanceOfChecks in the scalatest docs
    result shouldBe a[Map[_,_]]
    result should be(expected)
  }

  it should "use method asAttrMap to convert attributes to Map" in {
    val node = (foo \ "bar")(0)
    val result = node.attributes.asAttrMap
    val expected = Map(
      "type" -> "greet",
      "style" -> "pleasent",
      "target" ->"anyone"
    )

    result should be(expected)
  }

  it should "retrieve a single attribute from a scala.xml.MetaData object" in {
    val node = (foo \ "bar")(0)
    val expected = "pleasent"
    // scala.xml.MetaData
    val attrs = node.attributes
    // Some[String]
    val result = attrs.get("style")
    result.value.text should be(expected)
  }

  behavior of "child"

  it should "return the immediate child nodes" in {
    import scala.collection.mutable.ArrayBuffer
    val expected: Seq[Node] = ArrayBuffer[Node](
      <bar type="greet" style="pleasent" target="anyone">hi</bar>,
      <bar type="color">yellow</bar>,
      <baz>
        <bar type="bad">Bad</bar>
      </baz>
    )

    val result: Seq[Node] = foo.child

    result should contain(expected(0))
    result should contain(expected(1))
    // Unreliable assertion
    /*
    val a = <foo><bar>M</bar></foo>
    val b = <foo>
      <bar>M</bar>
    </foo>

    a == b -> False
    a equals b -> False

    a.child == b.child -> False
    a.child equals b.child -> False
     */
    result should contain(expected(2))
  }

  it should "allow use of map and filter" in {
    import scala.collection.mutable.ArrayBuffer

    val expected = ArrayBuffer(
      (
        "bar",
        Map(
          "type" -> "greet",
          "style" -> "pleasent",
          "target" -> "anyone"
        ),
        "hi"
      ),
      (
        "bar",
        Map("type" -> "count"),
        "1"
      ),
      (
        "bar",
        Map("type" -> "color"),
        "yellow"
      )
    )

    val result = foo.child.map(x => {
      x match {
        case <bar>{text}</bar> => (x.label, x.attributes.asAttrMap, text)
        case <baz>_</baz> => (x.label, Map[String,String](), "")
        case _ => ("", Map[String,String](), "")
      }
    })
    .filter(x => {
      x match {
        case ("bar", _, _) => true
        case _ => false
      }
    })

    result.toString should be(expected.toString)
    // result should contain allElementsOf expected
    // result should contain(expected(1))
  }

  behavior of "fromString"

  it should "read a String and convert it to an scala.xml.Elem" in {
    val str = """<foo>
      <bar type="greet" style="pleasent" target="anyone">hi</bar>
      <bar type="count">1</bar>
      <bar type="color">yellow</bar>
      <baz>
        <bar type="bad">Bad</bar>
      </baz>
    </foo>
    """

    val result = XML.loadString(str)
    result should be(foo)
  }

}
