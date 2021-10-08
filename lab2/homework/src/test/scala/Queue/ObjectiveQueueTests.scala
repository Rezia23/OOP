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
//    s.print()
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

  test("Filter") {
    val s1 = ObjectiveQueue(1).appended(2).appended(3).appended(4)
    val s = s1.filter(e => e %2 == 0)
    println(s.front)
    assert(!s.contains(1))
    assert(s.contains(2))
    assert(!s.contains(3))
    assert(s.contains(4))
  }

  test("Map") {
    val s1 = ObjectiveQueue(1).appended(2).appended(3).appended(4)
    val s = s1.map(e => (e * -1))
    assert(s.front == -1)
    assert(s.back == -4)
    assert(s.contains(-2))
    assert(s.contains(-3))
    assert(!s.contains(2))
    assert(!s.contains(1))
  }

  test("Forall") {
    val s1 = ObjectiveQueue(1).appended(2).appended(3).appended(4)
    assert(!s1.forall(e => e %2 == 0))
    val s2 = s1.filter(e => e %2 == 0)
    assert(s2.forall(e => e %2 == 0))
  }

  test("Foreach") {
    val s1 = ObjectiveQueue(1).appended(2).appended(3)
    var i = 0
    s1.foreach(e => {
      assert(e == i + 1)
      i += 1
    })
    assert(i == 3)
  }

}