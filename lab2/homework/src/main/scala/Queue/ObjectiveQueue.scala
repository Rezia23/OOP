package Queue
abstract class ObjectiveQueue
{
  /**
   * Returns true if the queue contains a number
   */
  def contains(number: Int): Boolean

  /**
   * Creates a new queue with a new number in the back
   */
  def appended(number: Int): ObjectiveQueue

  /**
   * Creates a new queue with a new number in the front
   */
  def prepended(number: Int): ObjectiveQueue

  /**
   * Returns the front number
   */
  def front: Int

  /**
   * Returns the back number
   */
  def back: Int

  /**
   * Return true if this queue is empty.
   * Else false.
   */
  def isEmpty: Boolean

  /**
   * Returns new queue without the front number
   */
  def poppedFront(): ObjectiveQueue

  /**
   * Returns new queue without the back number
   */
  def poppedBack(): ObjectiveQueue

  /**
   * Returns a new queue that contains numbers returning true from the predicate
   */
  def filter(predicate: Int => Boolean): ObjectiveQueue

  /**
   * Returns a new queue where all numbers from this queue
   * were transformed using a transform function
   */
  def map(transform: Int => Int): ObjectiveQueue

  /**
   * Returns true if all numbers from this queue return true from a predicate.
   * Else false
   */
  def forall(predicate: Int => Boolean): Boolean

  /**
   * Iterates all numbers from this queue and inputs them in a loopFunction
   */
  def foreach(loopFunction: Int => Unit): Unit
}