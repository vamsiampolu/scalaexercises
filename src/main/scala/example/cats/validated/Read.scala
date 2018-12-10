package example.cats.validated

/**
  *
  * @tparam A represents the data type to be read from input
  * Read is a typeclass that has two instances:
  *   - Int
  *
  *   - String
  */
trait Read[A] {
  def read(s: String): Option[A]
}

object Read {
  // this is the same as using implicitly
  def apply[A](implicit A: Read[A]) = A

  implicit val stringRead: Read[String] = new Read[String] {
    def read(s: String):Option[String] = {
      Some(s)
    }
  }

  implicit val intRead: Read[Int] = new Read[Int] {
    override def read(s: String): Option[Int] = {
      if (s.matches("-?[0-9]+")) Some(s.toInt)
      else None
    }
  }
}
