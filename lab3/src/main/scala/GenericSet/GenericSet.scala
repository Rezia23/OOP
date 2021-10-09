package GenericSet

abstract class ObjectiveSet {

  /**
   * Returns true if a number is in the set.
   * Else false.
   */
  def contains(item: Int): Boolean

  /**
   * Returns a new set that includes a number
   */
  def including(item: Int): ObjectiveSet

  /**
   * Returns a new set without a number
   */
  def excluding(item: Int): ObjectiveSet

  /**
   * Returns a new set that includes numbers from this set and a set
   */
  def union(set: ObjectiveSet): ObjectiveSet

  /**
   * Returns a new set that contains numbers that are in this set and the other set simultaneously
   */
  def intersection(set: ObjectiveSet): ObjectiveSet

  /**
   * Returns a new set that contains numbers from this set that are not in the other set
   */
  def difference(set: ObjectiveSet): ObjectiveSet

  /**
   * Returns a new set that contains numbers returning true from the predicate
   */
  def filter(predicate: Int => Boolean): ObjectiveSet

  /**
   * Returns a new set where all numbers from this set were transformed using a transform function
   */
  def map(transform: Int => Int): ObjectiveSet

  /**
   * Returns true if all numbers from this set return true from a predicate.
   * Else false.
   */
  def forall(predicate: Int => Boolean): Boolean

  /**
   * Iterates all numbers from this set and inputs them in a loopFunction
   */
  def foreach(loopFunction: Int => Unit): Unit

  /**
   * Prints out the contents of this tree
   */
  def print(): Unit
}

object ObjectiveSet{
  def apply(number: Int): ObjectiveSet = new ObjectiveSetNode(number, EmptyImmutableSetNode, EmptyImmutableSetNode)
}

class ObjectiveSetNode(val value: Int, left: ObjectiveSet, right: ObjectiveSet) extends ObjectiveSet
{

  override def contains(item: Int): Boolean = {
    if (item < value)
      left.contains(item)
    else if (item > value)
      right.contains(item)
    else
      true
  }

  override def including(item: Int): ObjectiveSet = {
    if (item == value)
      throw new IllegalArgumentException("Value already exists");

    if (item < value)
      new ObjectiveSetNode(value, left.including(item), right);
    else
      new ObjectiveSetNode(value, left, right.including(item));
  }

  override def excluding(item: Int): ObjectiveSet = {
    if (item < value)
      new ObjectiveSetNode(value, left.excluding(item), right)
    else if (item > value)
      new ObjectiveSetNode(value, left, right.excluding(item))
    else
      left.union(right)
  }

  override def union(set: ObjectiveSet): ObjectiveSet = {
    left.union(right.union(set)).including(value)
  }

  override def intersection(set: ObjectiveSet): ObjectiveSet = {
    var res = left.intersection(set).union(right.intersection(set));
    if (set.contains(value))
      res = res.including(value)
    res
  }

  override def difference(set: ObjectiveSet): ObjectiveSet = {
    if (!set.contains(value))
      new ObjectiveSetNode(value, left.difference(set), right.difference(set))
    else
      left.union(right).difference(set)
  }

  override def filter(predicate: Int => Boolean): ObjectiveSet = {
    if (predicate(value))
      new ObjectiveSetNode(value, left.filter(predicate), right.filter(predicate))
    else
      left.filter(predicate).union(right.filter(predicate))
  }

  override def map(transform: Int => Int): ObjectiveSet = {
    new ObjectiveSetNode(transform(value), left.map(transform), right.map(transform))
  }

  override def forall(predicate: Int => Boolean): Boolean = {
    predicate(value) && left.forall(predicate) && right.forall(predicate)
  }

  override def foreach(f: Int => Unit): Unit = {
    left.foreach(f)
    f(value)
    right.foreach(f)
  }

  override def print(): Unit = {
    left.print()
    println(value)
    right.print()
  }
}

object EmptyImmutableSetNode extends ObjectiveSet{
  override def contains(item: Int): Boolean = false

  override def including(item: Int): ObjectiveSet = {
    new ObjectiveSetNode(item, this, this)
  }

  override def excluding(item: Int): ObjectiveSet = this

  override def union(set: ObjectiveSet): ObjectiveSet = set

  override def intersection(set: ObjectiveSet): ObjectiveSet = this

  override def difference(set: ObjectiveSet): ObjectiveSet = this

  override def filter(predicate: Int => Boolean): ObjectiveSet = this

  override def map(transform: Int => Int): ObjectiveSet = this

  override def forall(predicate: Int => Boolean): Boolean = true

  override def foreach(f: Int => Unit): Unit = { }

  override def print(): Unit = { }
}
