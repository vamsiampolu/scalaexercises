package example.cats.validated;

/**
  * ConfigError is a type constructor with two value constructors
  *   - MissingConfig: this error instance is used when a config field is not provided
  *
  *   - ParseError
  */
sealed abstract class ConfigError

object ConfigError {
  final case class MissingConfig(field: String) extends ConfigError
  final case class ParseError(field: String) extends ConfigError
}
