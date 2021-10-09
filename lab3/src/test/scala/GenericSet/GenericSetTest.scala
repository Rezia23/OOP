package GenericSet

import org.scalatest.FunSuite

import scala.language.implicitConversions



class GenericSetTest extends FunSuite {
  test("Including and contains") {
    val s = ObjectiveSet[BigInt](1)
      .including[BigInt](2)
      .including[BigInt](3)
    assert(s.contains[BigInt](1))
    assert(s.contains[BigInt](2))
    assert(s.contains[BigInt](3))
  }

  test("Excluding") {
    val s = ObjectiveSet[BigInt](1)
      .including[BigInt](2)
      .including[BigInt](3)
    var s1 = s.excluding[BigInt](2).excluding[BigInt](1)
    assert(!s1.contains[BigInt](1))
    assert(!s1.contains[BigInt](2))
    assert(s1.contains[BigInt](3))
  }

  test("Intersection, difference"){
      val s1 = ObjectiveSet[BigInt](1, 2)
      val s2 = ObjectiveSet[BigInt](2, 3)
      val s = s1.intersection(s2)

      assert(!s.contains[BigInt](1))
      assert(s contains[BigInt] 2)
      assert(!(s contains[BigInt] 3))
      assert(!(s contains[BigInt] 4))

      val sa = ObjectiveSet[BigInt](1, 2)
      val sb = ObjectiveSet[BigInt](2, 3)
      val st = sa.difference(sb)

      assert(st.contains[BigInt](1))
      assert(!st.contains[BigInt](2))
      assert(!st.contains[BigInt](3))
      assert(!st.contains[BigInt](4))

  }
  test("Filter") {
    val s1 = ObjectiveSet[BigInt](1, 2, 3, 4)
    val s = s1.filter(e => e % 2 == 0)

    assert(!s.contains[BigInt](1))
    assert(s.contains[BigInt](2))
    assert(!s.contains[BigInt](3))
    assert(s.contains[BigInt](4))
  }

  test("Map") {
    val s1 = ObjectiveSet[BigInt](1, 2, 3)
    val s = s1.map[BigInt](e => e * 2)

    assert(!s.contains[BigInt](1))
    assert(s.contains[BigInt](2))
    assert(!s.contains[BigInt](3))
    assert(!s.contains[BigInt](3))
    assert(s.contains[BigInt](4))
    assert(!s.contains[BigInt](5))
    assert(s.contains[BigInt](6))
    assert(!s.contains[BigInt](7))
  }



}
