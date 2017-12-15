package example

import scala.xml._
import org.scalatest._

/* Reading List:
  Note: The author of the first blog post has not escaped \ and \\ and thus his source code samples show space instead of \ and \ instead of \\. This can be very confusing.
 
 * http://bcomposes.com/2012/05/04/basic-xml-processing-with-scala/
 
 * https://alvinalexander.com/scala/xml-parsing-xpath-extract-xml-tag-attributes
 
 * https://alvinalexander.com/scala/how-to-extract-data-from-xml-nodes-in-scala
 
 // Pattern Matching Lament taken from:
 * http://www.codecommit.com/blog/scala/working-with-scalas-xml-support
*/


case class Song(val title: String, val length: String) {
  lazy val time: Int = {
    val split = length split(":")
    split match {
      case Array(min, sec) => (min.toInt * 60) + sec.toInt
    }
  }
}

case class Album(val title: String, val songs: Seq[Song], val description: String) {
  lazy val time = {
    songs
    .map(x => { x.time })
    .sum
  }
  lazy val length = s"${(time / 60)}:${(time % 60)}"
}

case class Artist(val name: String, val albums: Seq[Album])

class XmlSpec extends FlatSpec with Matchers with BeforeAndAfter with OptionValues {

  var foo: Elem = _
  var music: Elem = _

  before {
    foo = <foo>
      <bar type="greet" style="pleasent" target="anyone">hi</bar>
      <bar type="count">1</bar>
      <bar type="color">yellow</bar>
      <baz>
        <bar type="bad">Bad</bar>
      </baz>
    </foo>

    music = XML.loadString("""
      <music>
        <artist name="Radiohead">
          <album title="The King of Limbs">
            <song title="Bloom" length="5:15"/>
            <song title="Morning Mr Magpie" length="4:41"/>
            <song title="Little by Little" length="4:27"/>
            <song title="Feral" length="3:13"/>
            <song title="Lotus Flower" length="5:01"/>
            <song title="Codex" length="4:47"/>
            <song title="Give Up the Ghost" length="4:50"/>
            <song title="Separator" length="5:20"/>
            <description link="http://en.wikipedia.org/wiki/The_King_of_Limbs">
            The King of Limbs is the eighth studio album by English rock band Radiohead, produced by Nigel Godrich. It was self-released on 18 February 2011 as a download in MP3 and WAV formats, followed by physical CD and 12" vinyl releases on 28 March, a wider digital release via AWAL, and a special "newspaper" edition on 9 May 2011. The physical editions were released through the band's Ticker Tape imprint on XL in the United Kingdom, TBD in the United States, and Hostess Entertainment in Japan.
          </description>
        </album>
        <album title="OK Computer">
          <song title="Airbag"  length="4:44"/>
          <song title="Paranoid Android"  length="6:23"/>
          <song title="Subterranean Homesick Alien"  length="4:27"/>
          <song title="Exit Music (For a Film)"  length="4:24"/>
          <song title="Let Down"  length="4:59"/>
          <song title="Karma Police"  length="4:21"/>
          <song title="Fitter Happier"  length="1:57"/>
          <song title="Electioneering"  length="3:50"/>
          <song title="Climbing Up the Walls"  length="4:45"/>
          <song title="No Surprises"  length="3:48"/>
          <song title="Lucky"  length="4:19"/>
          <song title="The Tourist"  length="5:24"/>
          <description link="http://en.wikipedia.org/wiki/OK_Computer">
          OK Computer is the third studio album by the English alternative rock band Radiohead, released on 16 June 1997 on Parlophone in the United Kingdom and 1 July 1997 by Capitol Records in the United States. It marks a deliberate attempt by the band to move away from the introspective guitar-oriented sound of their previous album The Bends. Its layered sound and wide range of influences set it apart from many of the Britpop and alternative rock bands popular at the time and laid the groundwork for Radiohead's later, more experimental work.
        </description>
      </album>
    </artist>
    <artist name="Portishead">
      <album title="Dummy">
        <song title="Mysterons"  length="5:02"/>
        <song title="Sour Times"  length="4:11"/>
        <song title="Strangers"  length="3:55"/>
        <song title="It Could Be Sweet"  length="4:16"/>
        <song title="Wandering Star"  length="4:51"/>
        <song title="It's a Fire"  length="3:49"/>
        <song title="Numb"  length="3:54"/>
        <song title="Roads"  length="5:02"/>
        <song title="Pedestal"  length="3:39"/>
        <song title="Biscuit"  length="5:01"/>
        <song title="Glory Box"  length="5:06"/>
        <description link="http://en.wikipedia.org/wiki/Dummy_%28album%29">
        Dummy is the debut album of the Bristol-based group Portishead. Released in August 22, 1994 on Go! Discs, the album earned critical acclaim, winning the 1995 Mercury Music Prize. It is often credited with popularizing the trip-hop genre and is frequently cited in lists of the best albums of the 1990s. Although it achieved modest chart success overseas, it peaked at #2 on the UK Album Chart and saw two of its three singles reach #13. The album was certified gold in 1997 and has sold two million copies in Europe. As of September 2011, the album was certified double-platinum in the United Kingdom and has sold as of September 2011 825,000 copies.
      </description>
    </album>
    <album title="Third">
      <song title="Silence"  length="4:58"/>
      <song title="Hunter"  length="3:57"/>
      <song title="Nylon Smile"  length="3:16"/>
      <song title="The Rip"  length="4:29"/>
      <song title="Plastic"  length="3:27"/>
      <song title="We Carry On"  length="6:27"/>
      <song title="Deep Water"  length="1:31"/>
      <song title="Machine Gun"  length="4:43"/>
      <song title="Small"  length="6:45"/>
      <song title="Magic Doors"  length="3:32"/>
      <song title="Threads"  length="5:45"/>
      <description link="http://en.wikipedia.org/wiki/Third_%28Portishead_album%29">
      Third is the third studio album by English musical group Portishead, released on 27 April 2008, on Island Records in the United Kingdom, two days after on Mercury Records in the United States, and on 30 April 2008 on Universal Music Japan in Japan. It is their first release in 10 years, and their first studio album in eleven years. Third entered the UK Album Chart at #2, and became the band's first-ever American Top 10 album on the Billboard 200, reaching #7 in its entry week.
    </description>
  </album>
</artist>
</music>
    """)
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
        case <bar>{node}</bar> => (x.label, x.attributes.asAttrMap, node.text)
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

    result should contain allElementsOf expected
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

  behavior of "pattern matching gotchas"

  it should "work correctly with an empty self-closing node" in {
    val input = <foo/>
    val expected = "foo"

    val result = input match {
      case <foo/> => expected
      case _ => "bar"
    }

    result should be(expected)
  }

  it should "throw a MatchError when matching non-empty tags as if they were empty self-closing tags" in {
    val input = <foo>bar</foo>

    lazy val result = input match {
      case <foo/> => "foo"
      case <bar/> => "bar"
    }
    a [scala.MatchError] should be thrownBy println(result)
  }

  it should "work correctly when exact xml node with text is specified" in {
    val input = <foo>bar</foo>
    val expected = "foobar"

    val result = input match {
      case <foo>bar</foo> => expected
      case <bar/> => "Bar"
    }

    result should be(expected)
  }

  it should "work with interpolation pattern on text" in {
    val input = <foo>bar</foo>
    val expected = "foobar"

    val result = input match {
      case <foo>{ node }</foo> => "foo" + node.text
      case <bar/> => "Bar"
    }

    result should be(expected)
  }

  it should "throw a MatchError when more than one element exists as a child of xml" in {
    val input = <foo>bar<baz/></foo>
    lazy val result = input match {
      case <foo>{ node }</foo> => "foo" + node.text
      case <bar/> => "bar"
    }

    a [scala.MatchError] should be thrownBy println(result)
  }

  it should "work correctly when using scala with pattern" in {
    val input = <foo>bar<baz/></foo>
    // same as NodeSeq.fromSeq(input.child) but looks nicer
    val expected = NodeSeq fromSeq { input child }

    val result = input match {
      case <foo>{ ns @ _* }</foo> => NodeSeq fromSeq(ns)
      case <bar/> => "bar"
    }

    result should be(expected)
  }

  it should "does not compile when matching on attributes" in {
    val input = <foo type="bar"/>
     """
      input match {
        case <foo type="bar"/> => "foobar"
        case <bar/> => "nope"
      }
     """ shouldNot compile
  }

  it should "work correctly when using guards with pattern matching" in {
    val input = <foo type="bar"/>
    val expected = "foobar"

    val result = input match {
      case n @ <foo/> if (n \ "@type" text) == "bar" => expected
      case <bar/> => "nope"
    }

    result should be(expected)
  }

  it should "match tag irrespective of attached namespace" in {
    val input = <w:foo type ="bar" />
    val expected = "Foo"
    val result = input match {
      case <foo /> => expected
      case <bar /> => "Bar"
    }
    result should be(expected)
  }

  behavior of "marshalling/unmarshalling XML"

  it should "convert XML elements to classes" in {
    val result = (music \\ "song")
      .map(song => {
      val title = (song \ "@title").text
      // length was actually returned as an Int
      val length:String = (song \ "@length").text.toString
      Song(title, length)
    })

    // List[Song] is discouraged, a future version of scalatest would treat that as a compiler error
    result shouldBe a[List[_]]

    result should contain(Song("Feral","3:13"))

    val len = result.map(x => { x.time }).sum
    len shouldBe 11311
  }

  it should "allow creating nested objects in XML" in {
    val artists = (music \ "artist").map { artist =>
      val name = (artist \ "@name").text
      val albums = (artist \ "album").map { album =>
        val title = (album \ "@title").text
        val description = (album \ "description").text
        val songList = (album \ "song").map { song =>
          Song((song \ "@title").text, (song \ "@length").text)
        }
        Album(title, songList, description)
      }
      Artist(name, albums)
    }

    val result = artists.flatMap { artist =>
      artist.albums.map(album => {
        (artist.name, album.title, album.length)
      })
    }

    val expected = List(
      ("Radiohead","The King of Limbs","37:34"),
      ("Radiohead","OK Computer","53:21"),
      ("Portished","Dummy","48:46"),
      ("Portished","Third","48:50")
    )
  }

  it should "allow converting from Scala to XML" in {

    val artists = (music \ "artist").map { artist =>
      val name = (artist \ "@name").text
      val albums = (artist \ "album").map { album =>
        val title = (album \ "@title").text
        val description = (album \ "description").text
        val songList = (album \ "song").map { song =>
          Song((song \ "@title").text, (song \ "@length").text)
        }
        Album(title, songList, description)
      }
      Artist(name, albums)
    }

    val result = <music>
      { artists.map { artist =>
        <artist name={artist.name}>
        { artist.albums.map { album =>
          <album title={album.title}>
          { album.songs.map(song => <song title={song.title} length={song.length}/>) }
          <description>{album.description}</description>
        </album>
        }}
      </artist>
      }}
    </music>

    // (music child) should contain atleastOneElementOf (result child)
    pending
  }
}
