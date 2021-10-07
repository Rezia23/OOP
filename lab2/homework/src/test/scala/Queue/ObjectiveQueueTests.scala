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
    assert(s.getReverseList == List(2,1))
  }

  test("Append and prepend"){
    var q = ObjectiveQueue(1).appended(2).appended(3).prepended(0)
    assert(q.getReverseList == List(3,2,1,0))
    q = q.prepended(-1).appended(4)
    assert(q.getReverseList == List(4,3,2,1,0,-1))
  }

  test("Front and back"){
    var q = ObjectiveQueue(1).appended(2)
    assert(q.front == 1)
    assert(q.back == 2)
    q = q.prepended(0)
    q = q.appended(3)
    assert(q.front == 0)
    assert(q.back == 3)
  }

  test("Popped front and popped back"){
    var q = ObjectiveQueue(1).appended(2)
    q = q.poppedFront()
    assert(q.front == 2)
    assert(q.back == 2)
    q = q.prepended(0)
    q = q.appended(3)
    q = q.poppedBack()
    assert(q.back == 2)
    q = q.poppedFront()
    q = q.poppedFront()
    assert(q.front == Int.MinValue)
    assert(q.back == Int.MinValue)
  }

}