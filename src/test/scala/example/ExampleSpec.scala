package example

import org.scalatest._ 
import scala.collection.mutable.Stack

class ExampleSpec extends FeatureSpec with GivenWhenThen with Matchers{
    feature("The user can pop an element of the stack") {

        info("As a programmer")
        info("I want to be able to pop items off the stack")
        info("So that I can get them in last-in-first-out order")

        scenario("pop is invoked by user on non-empty stack") {
            Given("a non-empty stack")
            val stack = new Stack[Int]
            stack.push(2)
            stack.push(5)
            val oldSize = stack.size

            When("pop is invoked on the stack")
            val result = stack.pop

            Then("most recently pushed element must be returned")
            result should be(5)

            And("the stack should have one less item than before")
            stack.size should be(1)
        }

        scenario("pop is invoked by user on empty stack") {
            Given("an empty stack")
            val stack = new Stack[Int]
            When("pop is invoked on the stack")
            Then("NoSuchElementException is thrown")
            a [NoSuchElementException] should be thrownBy stack.pop
            And("the stack should still be empty")
            stack.size should be(0)
        }
    }
}