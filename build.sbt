import Dependencies._
import sbt.Keys.libraryDependencies

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.example",
      scalaVersion := "2.12.4",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "Hello",
     libraryDependencies += sbbt,
     libraryDependencies += sttp_json4s,
     libraryDependencies ++= circe,
     libraryDependencies += circe_sttp,
    libraryDependencies += scalaTest % Test,
    libraryDependencies += scalaXml,
    libraryDependencies += nscalatime,
    libraryDependencies ++= json4s,
    libraryDependencies += xmlunit,
    libraryDependencies += scala_xml_diff % Compile,
    libraryDependencies += scalatest_json,
    libraryDependencies += cats,
    libraryDependencies += scalaredis
  )

resolvers += Resolver.sonatypeRepo("releases")

scalacOptions += "-Ypartial-unification"

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.8")
