package example

val addOne x: Int => Int =  x => x + 1
def addTwo(x:Int): Int = { x + 1 }

object Hello extends Greeting with App {
  println(greeting)
}

trait Greeting {
  lazy val greeting: String = "hello"
}
