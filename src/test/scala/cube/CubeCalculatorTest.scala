package cube

import org.scalatest.FunSuite

class CubeCalculatorTest extends FunSuite {
  test("CubeCalculator.cube") {
    val cc = new CubeCalculator() 
    assert(cc.cube(3) == 27)
  }
}
