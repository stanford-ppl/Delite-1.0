/* locally weighted linear regression
 *
 * author: Arvind Sujeeth (asujeeth@stanford.edu)
 * last modified: 7/8/09
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

package ppl.apps.ml.linreg

import ppl.delite.core.appinclude._
import ppl.delite.dsl.optiml.appinclude._
import ppl.delite.dsl.primitive.DeliteDouble
import ppl.delite.dsl.optiml._
import ppl.delite.dsl.optiml.io._
import ppl.delite.metrics._

import java.io.FileReader
import java.io.BufferedReader
import ppl.delite.core.{Delite, DeliteApplication}

object LinReg extends DeliteApplication {
  // unweighted linear regression using the normal equations
  // input: input training vector x
  //        output training vector y
  // output: predictions along uniformly sampled points
  def unweightedReg(x: Vector[Double], y: Vector[Double]) : Vector[Double] =
  {
    // by convention, x_0 = 1
    val X = Matrix(x.map(ele => Vector(1., ele)))

    // theta = inv(X.'X)*(X.'*y) (if y is a col vector)
    val theta = ((X.trans*X).inv)*(X.trans*y.trans)

    // the resulting fitted line is given by the equation
    //   h(x) = theta_0 + theta_1*x_1 + ...
    return theta
  }

  def weightedReg(x: Vector[Double], y: Vector[Double]) : Vector[Double] = {
    val tau = 10
    val X = Matrix(x.map(ele => Vector(1., ele)))

    // initialize prediction points
    val xstep : Double = 25.0/X.numRows
    val xref_pts = Vector.uniform(-10, xstep, 14.99).trans
    val xref = Matrix(xref_pts.map(ele => Vector(1., ele)))
    //val O = Matrix.identity(X.numRows)
    val Xt = X.trans

    // calculate predictions
    //TODO: investigate results of mutable internal data structures / "macro-op"
    val guess = (0::xref.numRows)( e => {
      val x_cur = xref(e,1)
      val weights = x.map(ele => math.exp(-.1*(x_cur-ele)*(x_cur-ele)/(2.0*tau*tau))/2.0)
      val W = Matrix.diag(weights.length, weights)
      val t1 = Xt*W
      val theta = ((t1*X).inv)*(t1*y) // relaxed v_prod, ignore is_row on y
      (theta.trans).dot[DeliteDouble](xref(e).trans).value
    })

    return guess
  }

  // file format is m lines with n floats per line, each float seperated by 2 spaces
  // (same as matlab .dat)
  def print_usage = {
    println("Usage: LinRegSerial <input vector file> <output vector file>")
    exit(-1)
  }

  def run(args: Array[String]) : Unit = {
    if (args.length < 2) print_usage

    Delite.init = true
    val x = MLInputReader.readVector(args(0))
    val y = MLInputReader.readVector(args(1))

//    logElapsed("Input Section Complete")
    Delite.init = false

    val theta = unweightedReg(x, y)

    val num = 10
    for (i <- 0 until num) {
      PerformanceTimer.start("LinReg")
      val guess = weightedReg(x, y)
      guess.force
      PerformanceTimer.stop("LinReg")
      PerformanceTimer.print("LinReg")
    }
//    logElapsed("Calculation Complete")

    //println("Unweighted linear regression")
    //println("theta: ")
    //theta.pprint
    //print("\n")

    //println("Locally weighted linear regression")
    //println("guess: ")
    //guess.pprint
    //print("\n")


    PerformanceTimer.save("LinReg")
  }
}
