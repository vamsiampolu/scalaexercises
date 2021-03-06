import sbt._

object Dependencies {
  lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.0.3"
  lazy val sbbt = "com.softwaremill.sttp" %% "core" % "1.1.1"
  lazy val sttp_json4s = "com.softwaremill.sttp" %% "json4s" % "1.1.1"

  val json4s_version = "3.6.0-M2"

  val circeVersion = "0.8.0"
  lazy val circe = Seq(
    "io.circe" %% "circe-core",
    "io.circe" %% "circe-generic",
    "io.circe" %% "circe-parser"
  ).map(_ % circeVersion)

  lazy val json4s = Seq(
    "org.json4s" %% "json4s-native",
    "org.json4s" %% "json4s-ext"
  ).map(_ % json4s_version)
  
  lazy val cats = (
    "org.typelevel" %% "cats-core" % "1.5.0"
  )

  lazy val circe_sttp = "com.softwaremill.sttp" %% "circe" % "1.1.1"

  lazy val scalaXml = "org.scala-lang" % "scala-xml" % "2.11.0-M4"

  lazy val nscalatime = "com.github.nscala-time" %% "nscala-time" % "2.18.0"

  lazy val xmlunit = "org.xmlunit" % "xmlunit-core" % "2.5.1"

  lazy val scala_xml_diff =  "com.github.andyglow" %% "scala-xml-diff" % "2.0.3"

  lazy val scalatest_json = "com.stephenn" %% "scalatest-json4s" % "0.0.2"

  lazy val scalaredis =  "net.debasishg" %% "redisclient" % "3.30"
}
