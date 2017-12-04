package example

import org.scalatest._

class TvSet {
  private var on: Boolean = false
  def isOn: Boolean = on
  def pressPowerButton() {
    on = !on
  }
}

class TvSetSpec extends FeatureSpec with GivenWhenThen with Matchers {
  info("As a TV Set Owner")
  info("I want to be able to turn the TV on and off")
  info("So I can watch TV when I want")
  info("And save energy when I'm not watching TV")

  feature("TV Power Button") {
    scenario("User presses Power Button when TV is OFF") {
      Given("A TV Set that is switched off")
      val tv = new TvSet
      tv.isOn shouldBe false

      When( "The TV Power Button is pressed")
      tv.pressPowerButton()

      Then("the TV should be switched on")
      tv.isOn shouldBe true
    }
    scenario("User presses Power Button when TV is ON") {
      Given("A TV Set that is switched on")
      val tv = new TvSet
      tv.pressPowerButton()
      tv.isOn shouldBe true

      When( "The TV Power Button is pressed")
      tv.pressPowerButton()

      Then("the TV should be switched off")
      tv.isOn shouldBe false
    }
  }
}
