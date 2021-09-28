import FB.fizzBuzzArray
import org.scalatest.FunSuite

class MainTest extends FunSuite {
  test("OK"){

  }

}


class FizzBuzzTests extends FunSuite {

  def NthFizzBuzzNumber(index: Int): String = fizzBuzzArray()(index)

  test("Numbers") {
    assert(NthFizzBuzzNumber(0) == "1");
    assert(NthFizzBuzzNumber(1) == "2");
  }

  test("Fizzes") {
    assert(NthFizzBuzzNumber(2) == "Fizz");
  }

  test("Buzzes") {
    assert(NthFizzBuzzNumber(4) == "Buzz");
  }

  test("FizzBuzzes") {
    assert(NthFizzBuzzNumber(14) == "FizzBuzz");
  }
}
