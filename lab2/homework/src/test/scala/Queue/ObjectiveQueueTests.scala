package Queue

import org.scalatest.FunSuite

class ObjectiveQueueTests extends FunSuite{
  test("Appended and contains"){
    val s = ObjectiveQueue(1).appended(2)
    assert(s contains 1)
    assert(s contains 2)
    assert(!(s contains 3))
  }

  test("Printing and reverse"){
    val s = ObjectiveQueue(1).appended(2)
    s.print()
    assert(s.getReverseList() == List(2,1))
  }

  test("Append and prepend"){
    var q = ObjectiveQueue(1).appended(2).appended(3).prepended(0)
    assert(q.getReverseList() == List(3,2,1,0))
    q = q.prepended(-1).appended(4)
    assert(q.getReverseList() == List(4,3,2,1,0,-1))
  }

}