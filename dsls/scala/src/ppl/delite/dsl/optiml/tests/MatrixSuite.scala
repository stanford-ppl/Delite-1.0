/* Unit tests for OptiML matrices.
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
import ppl.delite.dsl.optiml.{Vector,Matrix}
import ppl.delite.tests.DeliteTestSuite

class MatrixSuite extends DeliteTestSuite {

  var v : Vector[Double] = null.asInstanceOf[Vector[Double]]
  var m : Matrix[Double] = null.asInstanceOf[Matrix[Double]]
  var mb : Matrix[Double] = null.asInstanceOf[Matrix[Double]]
  var f : Double => Double = null.asInstanceOf[Double=>Double]

  private val randRef = new java.lang.ThreadLocal[scala.util.Random] {
    override def initialValue = new scala.util.Random(100)
  }

  @Before def initialize() {
    m = Matrix[Double](100,100)
    mb = Matrix[Double](100,100)

    for (i <- 0 until m.numRows){
      for (j <- 0 until m.numCols){
        m(i,j) = randRef.get.nextDouble*100
        mb(i,j) = randRef.get.nextDouble*100
      }
    }

    v = Vector[Double](100)
    for (i <- 0 until v.size){
      v(i) = randRef.get.nextDouble*100
    }

    f = a => a*2
  }



  @Test def verifyAccessors() {
    assert(m.numRows == 100)
    assert(m.numCols == 100)
    assert(m.size == m.numRows*m.numCols)

    val elem = m(92,10)
    assert(m(92,10) == elem)

    var row = m(37)
    assert(row.length == m.numCols)
    for (j <- 0 until m.numCols){
      assert(row(j) == m(37,j))
      assert(row(j) == m.flattened(37*m.numCols+j))
    }

    var col = m.getCol(52)
    assert(col.length == m.numRows)
    for (j <- 0 until m.numRows){
      assert(col(j) == m(j,52))
      assert(col(j) == m.flattened(j*m.numCols+52))
    }

    var mat = m.sliceRows(3,5)
    for (i <- 0 until mat.numRows){
      for (j <- 0 until mat.numCols){
        assert(mat(i,j) == m(i+3,j))
      }
    }  

  }

  @Test def verifyOperators() {
    var m_rand = Matrix.rand(2,2)
    assert(m_rand(0,0) != m_rand(0,1))
    assert(m_rand(0,0) != m_rand(1,0))
    assert(m_rand(0,0) != m_rand(1,1))
  }

  @Test def verifyUpdates() {
    var init_m = m.clone

    m(72,5) = 3.14
    assert(m(72,5) == 3.14)

    m(3) = v
    for (j <- 0 until m.numCols){
      assert(v(j) == m(3,j))
    }

    var rows = m.numRows
    m.insertRow(6,v)
    assert(m.numRows == rows+1)
    for (j <- 0 until m.numCols){
      assert(m(6,j) == v(j))
    }
    rows += 1

    m.insertAllRows(72,mb)
    assert(m.numRows == rows+mb.numRows)
    for (i <- 0 until mb.numRows){
      for (j <- 0 until mb.numCols){
        assert(m(i+72,j) == mb(i,j))
      }
    }
    rows += mb.numRows

    m = init_m.clone
    rows = m.numRows
    var cols = m.numCols
    m.insertCol(17,v)
    assert(m.numCols == cols+1)
    for (j <- 0 until m.numRows){
      assert(m(j,17) == v(j))
    }
    cols += 1

    m.insertAllCols(99,mb)
    assert(m.numCols == cols+mb.numCols)
    for (i <- 0 until mb.numRows){
      for (j <- 0 until mb.numCols){
        assert(m(i,j+99) == mb(i,j))
      }
    }
    cols += mb.numCols

    var s_row = m(20).clone
    m.removeRows(10,10)
    assert(m.numRows == rows-10)
    assert(s_row.cmp(m(10)).value == true)
    rows -= 10

    var s_col = m.getCol(23).clone
    m.removeCols(13,10)
    assert(m.numCols == cols-10)
    assert(s_col.cmp(m.getCol(13)).value == true)
    cols -= 10
  }

}