/* Test suite for potential problems with anonymous functions and closures. These
 * should be solved by the ClosureDependencyExtractor compiler plugin.
 *
 * author:   Arvind Sujeeth (asujeeth@stanford.edu)
 * created:  Mar 3, 2010
 * modified: Mar 3, 2010
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */
 
package ppl.delite.tests

import org.scalatest.junit.JUnitSuite
import org.junit.{Test, Before}

import ppl.delite.dsl.primitive.DeliteDouble
import ppl.delite.core.appinclude._
import ppl.delite.dsl.optiml.{Matrix, Vector}

class ClosurePluginSuite extends JUnitSuite {
  var v : Vector[Double] = null.asInstanceOf[Vector[Double]]
  var v2 : Vector[Double] = null.asInstanceOf[Vector[Double]]
  var m : Matrix[Double] = null.asInstanceOf[Matrix[Double]]
  var ans : Matrix[Double] = null.asInstanceOf[Matrix[Double]]

  @Before def initialize() {
    v = Vector.zeros(100)
    v2 = Vector.zeros(100)
    m = Matrix[Double](10,100)
    ans = Matrix.zeros(10,100)

    // same ans used for all tests
    for (i <- 0 until 10){
      ans(i) = ans(i)+i
    }
  }

  // this test should work without any compiler plugin magic
  @Test def immutableDep() {
    var i = 0
    while (i < 10){
      val b : Double = i
      m(i) = v.map(e => e+b)
      i += 1
    }

    assert(m.cmp(ans).value)
  }

  // anti-dependency in map on v2 needs to be extracted by plugin
  @Test def mutableDep() {
    var i = 0
    while (i < 10){
      // compiler should change map to:
      //m(i) = v.map(ppl.delite.core.DeliteFunc(e => e+v2(10), v2))
      m(i) = v.map(e => e+v2(10))
      i += 1
      v2(10) += 1
      // TODO: may need to insert sleep here to trigger the race's bad outcome
    }

    assert(m.cmp(ans).value)
  }

  // the correct "version" (reference) of b needs to be extracted by the plugin for the map
  @Test def closureValueCap() {
    var i = 0
    var b : Double = 0
    while (i < 10){
      m(i) = v.map(e => e+b)
      i += 1
      b = i
    }

    assert(m.cmp(ans).value)
  }
}