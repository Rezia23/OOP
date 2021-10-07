package Queue
//todo can you instantiate an empty q?
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

  def print(): Unit
  def getReverseList: List[Int]
}

object EmptyQueueNode extends ObjectiveQueue {

  override def contains(item:Int):Boolean = false
  override def appended(number: Int): ObjectiveQueue = {
    new ObjectiveQueueNode(number, this)
  }
  override def prepended(number: Int): ObjectiveQueue  = {
    new ObjectiveQueueNode(number, this)
  }

  override def front: Int = Int.MinValue

  override def back: Int = Int.MinValue

  override def isEmpty: Boolean = true

  override def poppedFront(): ObjectiveQueue = this

  override def poppedBack(): ObjectiveQueue = this

  override def filter(predicate: Int => Boolean): ObjectiveQueue = this

  override def map(transform: Int => Int): ObjectiveQueue = this

  override def forall(predicate: Int => Boolean): Boolean = true

  override def foreach(loopFunction: Int => Unit): Unit = {}

  override def print():Unit = {}
  override def getReverseList: List[Int] = { List[Int]()}
}

object ObjectiveQueue{
  def apply(number: Int): ObjectiveQueue = new ObjectiveQueueNode(number, EmptyQueueNode)
}

class ObjectiveQueueNode ( val value: Int, prev: ObjectiveQueue) extends ObjectiveQueue{
  override def contains(item:Int):Boolean = {
    item == value || prev.contains(item)
  }
  override def appended(number: Int): ObjectiveQueue = {
    new ObjectiveQueueNode(number, this)
  }

  override def prepended(number: Int): ObjectiveQueue  = {
    this.union(prev.prepended(number))
  }
  def union(q : ObjectiveQueue): ObjectiveQueue = {
    new ObjectiveQueueNode(this.value, q)
  }

  override def front: Int = {
    if(prev.isEmpty){
       return value
    }
    prev.front
  }

  override def back: Int = value

  override def isEmpty: Boolean = false

  override def poppedFront(): ObjectiveQueue = {
    if(prev.isEmpty){
      return EmptyQueueNode
    }
    this.union(prev.poppedFront())
  }

  override def poppedBack(): ObjectiveQueue = prev

  override def filter(predicate: Int => Boolean): ObjectiveQueue = this

  override def map(transform: Int => Int): ObjectiveQueue = this

  override def forall(predicate: Int => Boolean): Boolean = true

  override def foreach(loopFunction: Int => Unit): Unit = {}

  override def print() : Unit = {
    println(value)
    prev.print()
  }
  override def getReverseList: List[Int] = {
    value +: prev.getReverseList
  }
}

