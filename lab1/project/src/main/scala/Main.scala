import java.io.OutputStream
import scala.collection.immutable.Nil.map
/**
 * @param numberTranslator the higher order function that returns a 
 *                         `String` mapping of a given `Int` value 
 */
class CustomFizzBuzzer(numberTranslator: Int => String) {

  /**
   * Translates a list of numbers into their FizzBuzz variant.
   *
   * @param numbers a list of numbers (to FizzBuzz)
   * @return a list of Fizzed and Buzzed strings
   */
  def buzzToList(numbers: List[Int]): List[String] = {
    var resultList = List[String]()

    for(number <- numbers)
      resultList = resultList.appended(numberTranslator(number))

    resultList
  }

  /** Write FizzBuzzes numbers into a stream */
  def buzzToStream(numbers: List[Int], out: OutputStream): Unit = {
    for(a <- buzzToList(numbers))
      out.write((a + System.lineSeparator()).getBytes("UTF-8"))
  }
}

object Main extends App {
  def numberTranslator (number: Int): String = {
    /*
    "No branch" solution
     */

    val res = (number%3, number%5)
    val ret = res match {
      case (0,0) => "FizzBuzz"
      case (0, _) => "Fizz"
      case (_, 0) => "Buzz"
      case (_, _) => number.toString
    }
    ret
    /*
      Original solution
     */
//    if (number % 3 == 0 && number % 5 == 0)
//      "FizzBuzz"
//
//    else if (number % 3 == 0)
//      "Fizz"
//
//    else if (number % 5 == 0)
//      "Buzz"
//
//    else
//      number.toString
  }
  val buzzer = new CustomFizzBuzzer(numberTranslator);
  buzzer.buzzToStream(List.range(1, 101), System.out);

  /*
  No loop fizz buzz
   */
  def noLoop(): Unit = {
    (1 until 100).map(i => (i % 3, i % 5) match {
      case (0, 0) => "FizzBuzz"
      case (0, _) => "Fizz"
      case (_, 0) => "Buzz"
      case _ => i
    }).foreach(println)
  }
//  noLoop()
  (1 until 100).map(i => (i % 3, i % 5)).foreach(println)
}

