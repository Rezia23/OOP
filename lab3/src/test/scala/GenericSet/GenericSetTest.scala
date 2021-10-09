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

}
