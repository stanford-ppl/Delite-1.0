/* Unit tests for OptiML vectors.
 *
 * author:   Arvind Sujeeth (asujeeth@stanford.edu)
 * created:  Feb 22, 2010
 * modified: Feb 22, 2010
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */
 
package ppl.delite.dsl.optiml.tests

import org.scalatest.junit.JUnitSuite
import org.junit.Assert._
import org.junit.{Test, Before}
import ppl.delite.core.appinclude._
import ppl.delite.dsl.optiml.{RangeVector, Vector}
import ppl.delite.tests.DeliteTestSuite

class VectorSuite extends DeliteTestSuite {

  var v : Vector[Double] = null.asInstanceOf[Vector[Double]]
  var vb : Vector[Double] = null.asInstanceOf[Vector[Double]]
  var f : Double => Double = null.asInstanceOf[Double=>Double]
  var range_easy : Vector[Int] = null.asInstanceOf[Vector[Int]]
  var range_hard : Vector[Int] = null.asInstanceOf[Vector[Int]]

  private val randRef = new java.lang.ThreadLocal[scala.util.Random] {
    override def initialValue = new scala.util.Random(100)
  }

  @Before def initialize() {
    v = Vector[Double](1000)
    for (i <- 0 until v.size){
      v(i) = randRef.get.nextDouble*100
    }

    vb = Vector[Double](10)
    for (i <- 0 until vb.size){
      vb(i) = randRef.get.nextDouble*100
    }

    f = a => a*2

    range_easy = Vector.range(0, 1000)
    range_hard = Vector.range(11, 100, 2)
  }



  @Test def verifyAccessors() {
    assert(v.length == 1000)

    val elem = v(92)
    assert(v(92) == elem)

    val first = v.first
    assert(first == v(0))

    val last = v.last
    assert(last == v(v.length-1))

    val twenty = v.slice(30, 50)
    assert(twenty.length == 20)
    for (i <- 0 until twenty.length){
      assert(twenty(i) == v(i+30))
    }

    val firstTen = v.take(10)
    assert(firstTen.length == 10)
    for (i <- 0 until firstTen.length){
      assert(firstTen(i) == v(i))
    }

    val allExceptTen = v.drop(10)
    assert(allExceptTen.length == (v.length - 10))
    for (i <- 0 until allExceptTen.length){
      assert(allExceptTen(i) == v(i+10))
    }
  }

  @Test def verifyOperators() {
    val vt = v.trans
    assert(vt.is_row != v.is_row)

    val vc = v.clone
    assert(vc.cmp(v).value == true)
  }

  @Test def verifyUpdates() {
    v(7) = 0.9123
    assert(v(7) == 0.9123)

    val twov = (v ++ v)
    assert(twov.length == v.length*2)
    assert(twov(1000) == v(0))

    var vlen = v.length
    v += 9.2
    assert(v.length == vlen + 1)
    assert(v(vlen) == 9.2)
    vlen += 1

    v ++= vb
    assert(v.length == vlen+vb.length)
    vlen += vb.length
    for (i <- 0 until vb.length){
      assert(v(vlen-vb.length+i) == vb(i))
    }

    v.copyFrom(100, vb)
    for (i <- 0 until vb.length){
      assert(v(i+100) == vb(i))
    }

    v.insert(500, 9.21)
    assert(v.length == vlen+1)
    assert(v(500) == 9.21)
    vlen += 1

    v.insertAll(13, vb)
    assert(v.length == vlen + vb.length)
    for (i <- 0 until vb.length){
      assert(v(i+13) == vb(i))
    }
    vlen += vb.length

    var shifted = v(72)
    v.remove(71)
    assert(v.length == vlen-1)
    assert(v(71) == shifted)
    vlen -= 1

    shifted = v(102)
    v.removeAll(99,3)
    assert(v.length == vlen-3)
    assert(v(99) == shifted)
    vlen -= 3

    v.trim
    assert(v.length == vlen)

    // TODO: the delite boolean should be implicitly downconverted to boolean
    assert(v.cmp(v).value == true)
  }

  @Test def verifyRange() {
    assert(range_easy(0) == 0)
    assert(range_easy(500) == 500)
    assert(range_easy(999) == 999)
    assert(range_easy.length == 1000)

    assert(range_hard(0) == 11)
    assert(range_hard(1) == 13)
    assert(range_hard(2) == 15)
    assert(range_hard(44) == 99)
    assert(range_hard.length == 45)
  }

}