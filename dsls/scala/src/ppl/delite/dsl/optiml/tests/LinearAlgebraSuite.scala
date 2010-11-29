package ppl.delite.dsl.optiml.tests

import ppl.delite.core.appinclude._
import ppl.delite.dsl.primitive.DeliteDouble
import ppl.delite.dsl.optiml.{Matrix, Vector}
import ppl.delite.dsl.optiml.appinclude._
import org.scalatest.junit.JUnitSuite
import ppl.delite.core.DeliteApplication
import org.junit.{After, Test, Before}
import ppl.delite.tests.DeliteTestSuite

class LinearAlgebraSuite extends DeliteTestSuite {

  var rowA : Vector[Double] = null.asInstanceOf[Vector[Double]]
  var rowB : Vector[Double] = null.asInstanceOf[Vector[Double]]
  var rowD : Vector[Double] = null.asInstanceOf[Vector[Double]]
  var colC : Vector[Double] = null.asInstanceOf[Vector[Double]]
  var colE : Vector[Double] = null.asInstanceOf[Vector[Double]]
  var m33 : Matrix[Double] = null.asInstanceOf[Matrix[Double]]
  var m23 : Matrix[Double] = null.asInstanceOf[Matrix[Double]]
  var m32 : Matrix[Double] = null.asInstanceOf[Matrix[Double]]
  val alpha = 4.235
  val beta = -99.759

  @Before def initialize() {
    rowA = Vector(true, 11., 22., 33.)
    rowB = Vector(true, -5.3, -17.2, -131.)
    rowD = Vector(true, -1.1, -6.2)
    colC = Vector(false,7., 3.2, 13.3)
    colE = Vector(false,.05, 9.97)
    // TODO: bug in optiml constructor (needs to be lifted), this shouldn't require an explicit force
    m33 = Matrix(rowA, rowB, colC.trans.force.asInstanceOf[Vector[Double]])
    m23 = Matrix(Vector(3.5, 7.5, 9.0), Vector(-5.6, 8.2, 17.3))
    m32 = Matrix(Vector(.07, .91), Vector(17., -10.), Vector(-99.,.023))
  }

  @Test def SimpleVectorArithmetic() {
    // outputs
    var ans_vec : Vector[Double] = null
    var ans_dbl : Double = 0.0
    var ans_mat : Matrix[Double] = null

    // A*B piecewise
    ans_vec = rowA*rowB
    assert(check(ans_vec, Vector(true, -58.3, -378.4, -4323.)))

    // dot product
    ans_dbl = rowA.dot[DeliteDouble](colC)
    assert(check(ans_dbl, 586.3))

    // outer product
    ans_mat = colC.outer(rowA)
    assert(check(ans_mat, Matrix(Vector(77., 154., 231.), Vector(35.2, 70.4, 105.6), Vector(146.3, 292.6, 438.9))))
  }

  @Test def SimpleMatrixArithmetic() {
    var ans_vec : Vector[Double] = null
    var ans_dbl : Double = 0.0
    var ans_mat : Matrix[Double] = null

    // matrix square multiplication
    ans_mat = m33*m33
    assert(check(ans_mat, Matrix(Vector(235.4, -30.8, -2080.1), Vector(-884.14, -239.96, 336.),
                                 Vector(153.14, 141.52, -11.31))))

    // inverse
    ans_mat = m33.inv
    assert(check(ans_mat, Matrix(Vector(-.0145, 0.0143, 0.1765), Vector(0.0645, 0.0065, -0.0965),
                                 Vector(-0.0079, -0.0091, 0.0055))))

    // matrix transpose
    ans_mat = m33.trans
    assert(check(ans_mat, Matrix(Vector(11., -5.3, 7.), Vector(22., -17.2, 3.2), Vector(33., -131., 13.3))))

    // matrix multiplication
    ans_mat = m33*m32
    assert(check(ans_mat, Matrix(Vector(-2892.223, -209.2310), Vector(12676.229, 164.1640), Vector(-1261.81, -25.3241))))

    // chained matrix multiplication
    ans_mat = m23*m33*m32
    assert(check(ans_mat, Matrix( Vector(73592.6225, 271.0046), Vector(98312.252799, 2079.73147))))

    // summation
    val ans = m23.sum[DeliteDouble].value
    assert(check(ans, 39.9))
  }

  @Test def combinedVecMatArithmetic() {
    var ans_vec : Vector[Double] = null
    var ans_dbl : Double = 0.0
    var ans_mat : Matrix[Double] = null

    ans_vec = m23*colC
    assert(check(ans_vec, Vector(true,168.2, 217.13)))

    ans_vec = rowB*m32
    assert(check(ans_vec, Vector(true,12676.229, 164.1640)))

    val a1 = m23*alpha
    val a2 = a1 * (m33.trans.inv)
    val a3 = a2 * m32
    ans_vec = a3 * (rowD.trans*colE*beta)
    assert(check(ans_vec, Vector(true,194179.526, 593097.843)))
  }

  ////////////////
  // helpers

  def approx(x: Double, y: Double) : Boolean = {
    // be very generous w.r.t. precision, because the ground truth
    // answers have not all been entered with high precision
    Math.abs(x - y) < .01
  }

  def check(x: Vector[Double], y: Vector[Double]) : Boolean = {
    if (x.length != y.length) return false
    for (i <- 0 until x.length)
      if (!approx(x(i),y(i))) return false

    return true
  }

  def check(x: Matrix[Double], y: Matrix[Double]) : Boolean = {
    if ((x.numRows != y.numRows) || (x.numCols != y.numCols)) return false
    for (i <- 0 until x.numRows)
      for (j <- 0 until x(i).length)
        if (!approx(x(i,j),y(i,j))) return false

    return true
  }

  def check(x: Double, y: Double) : Boolean = {
    approx(x,y)
  }
}