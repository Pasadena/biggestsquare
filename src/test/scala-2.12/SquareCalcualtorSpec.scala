import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest._

class SquareCalculatorSpec extends FlatSpec {

  "BiggestSquare" should "return 25 when list contain values (1,2,3,4,5,6,7,8,9)" in {
    val testService = new SquareCalculator
    val result = testService.findBiggestSquare(List(1,2,3,4,5,6,7,8,9))
    assert(result == 25)
  }

  "BiggestSquare" should "return 25 when list contain values (9,8,7,6,5,4,3,2,1)" in {
    val testService = new SquareCalculator
    val result = testService.findBiggestSquare(List(1,2,3,4,5,6,7,8,9))
    assert(result == 25)
  }

  "BiggestSquare" should "return 16 when list contain values (2,8,3,7,4,5,8,2,9)" in {
    val testService = new SquareCalculator
    val result = testService.findBiggestSquare(List(2,8,3,7,4,5,8,2,9))
    assert(result == 16)
  }

}
