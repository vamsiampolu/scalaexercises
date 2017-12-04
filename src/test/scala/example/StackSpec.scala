package example

import org.scalatest.FeatureSpec
import org.scalatest.GivenWhenThen
 
class StackSpec extends FeatureSpec with GivenWhenThen {
 
  feature("The user can pop an element off the top of the stack") {
 
    info("As a programmer")
    info("I want to be able to pop items off the stack")
    info("So that I can get them in last-in-first-out order")
 
    scenario("pop is invoked on a non-empty stack") {
 
      given("a non-empty stack")
      when("when pop is invoked on the stack")
      then("the most recently pushed element should be returned")
      and("the stack should have one less item than before")
      pending
    }
 
    scenario("pop is invoked on an empty stack") {
 
      given("an empty stack")
      when("when pop is invoked on the stack")
      then("NoSuchElementException should be thrown")
      and("the stack should still be empty")
      pending
    }
  }
}
