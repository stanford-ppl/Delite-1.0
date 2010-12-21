package ppl.apps.tests

/**
 * author: Michael Wu (mikemwu@stanford.edu)
 * last modified: 12/09/2010
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

import org.scalatest.junit.JUnitSuite
import org.junit.Test
import ppl.apps.ml.lbp.{BinaryFactor, UnaryFactor}

class FactorTest extends JUnitSuite {
  @Test
  def factors = {
    var uf = new UnaryFactor(1, 3)
    var uf2 = new UnaryFactor(2, 3)

    for(i <- 0 until 3) {
      uf.data(i) = i
      uf2.data(i) = 2 - i
    }

    /* var copy = uf.copy()

    copy.times(uf2)
    println(copy)

    copy = uf.copy()

    copy.divide(uf2)
    println(copy)

    copy = uf.copy()

    copy.plus(uf2)
    println(copy)

    copy = uf.copy()

    copy.damp(uf2, 0.5)
    println(copy)

    copy = uf.copy()

    println(copy.residual(uf2))   */

    var bf = new BinaryFactor(1, 3, 2, 3)
    bf.setLaplace(1)

    uf.convolve(bf, uf2)
    println(uf)
    
    assert(true)
  }
}