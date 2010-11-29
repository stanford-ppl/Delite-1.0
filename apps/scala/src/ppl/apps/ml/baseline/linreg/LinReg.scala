package ppl.apps.ml.baseline.linreg

import ppl.apps.ml.baseline.collection.{MLInputReader, DoubleMatrix, DoubleVector}
import ppl.delite.metrics.PerformanceTimer

/* locally weighted linear regression
 *
 * author: Arvind Sujeeth (asujeeth@stanford.edu)
 * last modified: 7/8/09
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

object LinReg {

  // unweighted linear regression using the normal equations
  // input: input training vector x
  //        output training vector y
  // output: predictions along uniformly sampled points
  def unweightedReg(x: DoubleVector, y: DoubleVector) : DoubleVector =
  {
    // by convention, x_0 = 1
    val X = DoubleMatrix(1, x)

    // theta = inv(X.'X)*(X.'*y) (if y is a col vector)
    val theta = ((X.trans*X).inv)*(X.trans*y.trans)

    // the resulting fitted line is given by the equation
    //   h(x) = theta_0 + theta_1*x_1 + ...
    return theta
  }

  def weightedReg(x: DoubleVector, y: DoubleVector) : DoubleVector = {
    val tau = 10
    val X = DoubleMatrix(1, x)

    // initialize prediction points
    val xstep : Double = 25.0/X.height
    val xref_pts = DoubleVector.uniform(-10, xstep, 14.99).trans
    val xref = DoubleMatrix(1, xref_pts)
    //val O = DoubleMatrix.identity(X.height)
    val Xt = X.trans

    // calculate predictions
    val guess = DoubleVector(xref.height)
    for (e <- 0 until xref.height){
      var x_cur = xref(e,1)
      var weights = x.map(ele => math.exp(-.1*math.pow(x_cur-ele,2)/(2.0*math.pow(tau,2)))/2.0)
      var W = DoubleMatrix.diag(weights.length, weights)
      val t1 = Xt*W
      var theta = ((t1*X).inv)*(t1*y)
      guess(e) = (theta.trans).dot(xref(e).trans)
    }

    return guess
  }

  // file format is m lines with n floats per line, each float seperated by 2 spaces
  // (same as matlab .dat)
  def print_usage = {
    println("Usage: LinRegSerial <input vector file> <output vector file>")
    exit(-1)
  }

  def main(args: Array[String]) : Unit = {
    println("LinReg Baseline")
    if (args.length < 2) print_usage

    val x = MLInputReader.readDoubleVector(args(0))
    val y = MLInputReader.readDoubleVector(args(1))

    val numTimes = 10
    for (i <- 0 until numTimes) {
      PerformanceTimer.start("LinRegbaseline")
      val guess = weightedReg(x, y)
      PerformanceTimer.stop("LinRegbaseline")
      PerformanceTimer.print("LinRegbaseline")
    }

    PerformanceTimer.save("LinRegbaseline")
  }

}