package example.cats.validated

import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import example.cats.validated.ConfigError.{MissingConfig, ParseError}

case class Config(map: Map[String, String]) {
  def parse[A: Read](key: String): Validated[ConfigError, A] = {
    map.get(key) match {
      case None => Invalid(MissingConfig(key))
      case Some(a) => Read[A].read(a) match {
        case None => Invalid(ParseError(key))
        case Some(value) => Valid(value)
      }
    }
  }
}
