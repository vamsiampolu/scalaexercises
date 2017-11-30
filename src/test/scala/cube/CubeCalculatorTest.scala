package cube

import org.scalatest.FunSuite
import org.scalatest.Matchers

class CubeCalculatorTest extends FunSuite with Matchers {
  test("CubeCalculator.cube") {
    val cc = new CubeCalculator() 
    val result = cc.cube(3)
     result should be(27)
  }
}
