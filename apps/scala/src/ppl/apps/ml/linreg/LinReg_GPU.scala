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
import ppl.delite.dsl.primitive.DeliteDouble
import ppl.delite.dsl.optiml.io.MLInputReader
import ppl.delite.dsl.optiml._
import ppl.delite.metrics._

import java.io.FileReader
import java.io.BufferedReader
import ppl.delite.core.{Delite, DeliteApplication}
import ppl.delite.core.executor.DeliteGPUThreadPool

object LinReg_GPU extends DeliteApplication {
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
    val guess_indx = Vector.range(0, xref.numRows).trans
    //val O = Matrix.identity(X.numRows).toDouble
    val Xt = X.trans
    
    // calculate predictions
    //TODO: investigate results of mutable internal data structures / "macro-op"
    val guess = Vector[Double](guess_indx.length)
    var e = 0
    while(e < guess_indx.length) {
      val x_cur = xref(e,1)
      val weights = x.mapLR(ele => Math.exp(-.1*Math.pow(x_cur-ele,2)/(2.0*Math.pow(tau,2))))(x_cur, tau)

      // Not using customized map operation
      //val x_cur = Vector.range(0,x.length).map(ele => xref(e,1)).toDouble
      //val scalar = 1.0/(-10*2.0*Math.pow(tau,2))
      //val weights = ((x_cur-x)*(x_cur-x)*(scalar)).exp
      val W = Matrix.diag(x.length, weights)
      //val W = O.dot(weights)
      val t1 = (Xt* W)
      val theta = ((t1*X).inv)*(t1*y) // relaxed v_prod, ignore is_row on y
      guess := (e, theta.trans.dot[DeliteDouble](xref(e).trans).value)
      e += 1
    }
    return guess
  }

  // file format is m lines with n floats per line, each float seperated by 2 spaces
  // (same as matlab .dat)
  def print_usage = {
    println("Usage: LinReg_GPU <input vector file> <output vector file>")
    exit(-1)
  }

  def run(args: Array[String]) : Unit = {
    if (args.length < 2) print_usage

    Delite.init = true
    val x = MLInputReader.readVector(args(0))
    val y = MLInputReader.readVector(args(1))

    Delite.init = false

    //val theta = unweightedReg(x, y)

    val num = 10
    for (i <- 0 until num) {
      DeliteGPUThreadPool.threads(0).cleanup_done = false
      DeliteGPUThreadPool.threads(0).cleanup = true
      while(!DeliteGPUThreadPool.threads(0).cleanup_done) {}
      
      PerformanceTimer.start("LinRegGPU")
      val guess = weightedReg(x, y)
      guess.force
      PerformanceTimer.stop("LinRegGPU")
      PerformanceTimer.print("LinRegGPU")
      //if(i==(num-1))
      //  guess.pprint
    }
    PerformanceTimer.save("LinRegGPU")
  }
}
