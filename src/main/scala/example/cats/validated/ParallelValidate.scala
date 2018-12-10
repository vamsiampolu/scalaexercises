package example.cats.validated

import cats.data.Validated.{Invalid, Valid}
import cats.data.{NonEmptyList, Validated}
import cats.{Semigroup, SemigroupK, Applicative}

object ParallelValidate {
  implicit val nelSemigroup: Semigroup[NonEmptyList[ConfigError]] =
    SemigroupK[NonEmptyList].algebra[ConfigError]

  implicit def validatedApplicative[E: Semigroup]: Applicative[Validated[E, ?]] = new Applicative[Validated[E, ?]]() {
    def ap[A,B](f: Validated[E, A => B])(fa: Validated[E,A]) = {
      (fa, f) match {
        case (Valid(a), Valid(fab)) => Valid(fab(a))
        case (i @ Invalid(_), Valid(_)) => i
        case (Valid(_), i @ Invalid(_)) => i
        case (Invalid(e1), Invalid(e2)) => Invalid(Semigroup[E].combine(e1, e2))
      }
    }

    override def pure[A](x: A): Validated[E, A] = Validated.valid(x)
    override def map[A,B](fa: Validated[E,A])(f: A => B): Validated[E, B] = fa.map(f)
    override def product[A, B](fa: Validated[E,A], fb: Validated[E, B]): Validated[E, (A, B)] = {
      ap(fa.map(a => (b:B) => (a,b)))(fb)
    }
  }

  implicit val readString: Read[String] = Read.stringRead
  implicit val readInt: Read[Int] = Read.intRead

  /**
    * If each piece of data is independent, we need to perform parallel validation.
    * We can validate that each piece of data is independent by asking for each of it upfront.
    * If only one of the two pieces of data is valid, we can use pattern matching but if both of
    * the values are invalid, the issue needs to be handled differently.
    *
    * A Semigroup represents an associative binary operation, the  function to be implemented
    * by instances of the Semigroup typeclass is called `combine`.
    *
    * When implementing a `combine` on ConfigError, we need to make first type parameter a
    * List[ConfigError] for Validated. It is common to use a `NonEmptyList[Error]`
    * as it guarantees that when `Invalid`, we should have atleast one error.
    *
    * This is so common that `Validated` has a `toValidatedNel` to convert a `Validated[E,A]` to a
    * `Validated[NonEmptyList[E], A]`
    *
    * The type `Validated[NonEmptyList[E], A]` has been aliased to `ValidatedNel[E, A]`
    *
    * The goal here is to replace `parallelValidate` here with `mapN` which is more generic.
    *
    * The type signature for `parallelValidate` is similar to `Apply#map2` which looks like this:
    *
    *  def map2[F[_], A, B, C](fa: F[A], fb: F[B])(f : (A, B) => C): F[C]
    *
    *  By defining `Applicative` on `Validated`, we get access to `|@\` cartesian syntax and the `map{2-22}`
    *  function which can be used instead of `parallelValidate`
    *
    *  Validated is not a Monad because:
    *
    *    Monad defines `ap` in terms of `flatMap`
    *
    *    (need to understand this better) the definition of ap from `Monad` is inconsistent.
    *
    *    Thus `Validated` can only be an Applicative
    *
    *    Validated#andThen can be used to perform sequential validation instead of parallelValidation
    *    The signature of andThen is similar to that of `flatMap`
    *
    *    Validated#withEither provides us with ability to fail on an individual error.
    *
    * @param v1
    * @param v2
    * @param f
    * @tparam E
    * @tparam A
    * @tparam B
    * @tparam C
    * @return
    */
  def parallelValidate[E: Semigroup, A, B, C](v1: Validated[E, A], v2: Validated[E, B])(f: (A, B) => C): Validated[E, C] = {
    (v1, v2) match {
      case (Valid(a), Valid(b)) => Valid(f(a, b))
      case (Valid(_),i @ Invalid(_)) => i
      case (i @Invalid(_), Valid(_)) => i
      case (Invalid(e1), Invalid(e2)) => Invalid(Semigroup[E].combine(e1, e2))
    }
  }
}
