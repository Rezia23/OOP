package GenericSet

abstract class ObjectiveSet[+T] {

  /**
   * Returns true if a number is in the set.
   * Else false.
   */
  def contains[S >: T<: Ordered[S]](item: S): Boolean

  /**
   * Returns a new set that includes a number
   */
  def including[S >: T <: Ordered[S]](item: S): ObjectiveSet[S]

  /**
   * Returns a new set without a number
   */
  def excluding[S >: T <: Ordered[S]](item: S): ObjectiveSet[T]

  /**
   * Returns a new set that includes numbers from this set and a set
   */
  def union[S >: T <: Ordered[S]](set: ObjectiveSet[S]): ObjectiveSet[S]

  /**
   * Returns a new set that contains numbers that are in this set and the other set simultaneously
   */
  def intersection[S >: T <: Ordered[S]](set: ObjectiveSet[S]): ObjectiveSet[S]

  /**
   * Returns a new set that contains numbers from this set that are not in the other set
   */
  def difference[S >: T <: Ordered[S]](set: ObjectiveSet[S]): ObjectiveSet[S]

  /**
   * Returns a new set that contains numbers returning true from the predicate
   */
  def filter(predicate: T => Boolean): ObjectiveSet[T]

  /**
   * Returns a new set where all numbers from this set were transformed using a transform function
   */
  def map[S >: T <: Ordered[S]](transform: T => S): ObjectiveSet[S]

//  /**
//   * Returns true if all numbers from this set return true from a predicate.
//   * Else false.
//   */
//  def forall(predicate: Int => Boolean): Boolean
//
//  /**
//   * Iterates all numbers from this set and inputs them in a loopFunction
//   */
//  def foreach(loopFunction: Int => Unit): Unit
//
//  /**
//   * Prints out the contents of this tree
//   */
//  def print(): Unit
}

object ObjectiveSet{
    def apply[T <: Ordered[T]](items: T*): ObjectiveSet[T] = {
      var resultSet: ObjectiveSet[T] = EmptySet

      for(item <- items)
      resultSet = resultSet.including(item)

      resultSet
    }

}

class ObjectiveSetNode[T <:Ordered[T]](val value: T, left: ObjectiveSet[T], right: ObjectiveSet[T]) extends ObjectiveSet[T]
{

  override def contains[S >: T <: Ordered[S]](item: S): Boolean = {
    if (item < value)
      left.contains(item)
    else if (item > value)
      right.contains(item)
    else
      true
  }

  override def including[S >: T<: Ordered[S]](item: S): ObjectiveSet[S] = {
    if (item == value)
      throw new IllegalArgumentException("Value already exists");

    if (item < value)
      new ObjectiveSetNode(value, left.including(item), right);
    else
      new ObjectiveSetNode(value, left, right.including(item));
  }

  override def excluding[S >: T <: Ordered[S]](item: S): ObjectiveSet[T] = {
    if (item < value)
      new ObjectiveSetNode(value, left.excluding(item), right)
    else if (item > value)
      new ObjectiveSetNode(value, left, right.excluding(item))
    else
      left.union(right)
  }

  override def union[S >:T <: Ordered[S]](set: ObjectiveSet[S]): ObjectiveSet[S] = {
    left.union(right.union(set)).including(value)
  }

  override def intersection[S >:T <: Ordered[S]](set: ObjectiveSet[S]): ObjectiveSet[S] = {
    var res = left.intersection(set).union(right.intersection(set));
    if (set.contains(value))
      res = res.including(value)
    res
  }

  override def difference[S >:T <: Ordered[S]](set: ObjectiveSet[S]): ObjectiveSet[S] = {
    if (!set.contains(value))
      new ObjectiveSetNode(value, left.difference(set), right.difference(set))
    else
      left.union(right).difference(set)
  }

  override def filter(predicate: T => Boolean): ObjectiveSet[T] = {
    if (predicate(value))
      new ObjectiveSetNode(value, left.filter(predicate), right.filter(predicate))
    else
      left.filter(predicate).union(right.filter(predicate))
  }

  override def map[S >: T <: Ordered[S]](transform: T => S): ObjectiveSet[S] = {
    new ObjectiveSetNode(transform(value), left.map(transform), right.map(transform))
  }
//
//  override def forall(predicate: Int => Boolean): Boolean = {
//    predicate(value) && left.forall(predicate) && right.forall(predicate)
//  }
//
//  override def foreach(f: Int => Unit): Unit = {
//    left.foreach(f)
//    f(value)
//    right.foreach(f)
//  }
//
//  override def print(): Unit = {
//    left.print()
//    println(value)
//    right.print()
//  }
}

object EmptySet extends ObjectiveSet[Nothing]{
  override def contains[S >: Nothing](item: S): Boolean = false

  override def including[S >:Nothing <: Ordered[S]](item: S): ObjectiveSet[S] = {
    new ObjectiveSetNode(item, this, this)
  }

  override def excluding[S >:Nothing <: Ordered[S]](item: S): ObjectiveSet[Nothing] = this

  override def union[S >:Nothing <: Ordered[S]](set: ObjectiveSet[S]): ObjectiveSet[S] = set

  override def intersection[S >: Nothing <: Ordered[S]](set: ObjectiveSet[S]): ObjectiveSet[S] = this

  override def difference[S >: Nothing <: Ordered[S]](set: ObjectiveSet[S]): ObjectiveSet[S] = this

  override def filter(predicate: Nothing => Boolean): ObjectiveSet[Nothing] = this

  override def map[S<: Ordered[S]](transform: Nothing => S): ObjectiveSet[Nothing] = this
//
//  override def forall(predicate: Int => Boolean): Boolean = true
//
//  override def foreach(f: Int => Unit): Unit = { }
//
//  override def print(): Unit = { }
}
