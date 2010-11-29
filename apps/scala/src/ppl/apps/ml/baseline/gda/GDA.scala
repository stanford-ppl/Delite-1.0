package ppl.apps.ml.baseline.gda

import ppl.apps.ml.baseline.collection.MLInputReader._
import ppl.delite.metrics.PerformanceTimer
import ppl.apps.ml.baseline.collection.{BooleanVector, DoubleMatrix, DoubleVector}

object GDA {

  def print_usage = {
    println("Usage: GDA <input data file> <output label data file>")
    exit(-1)
  }

  def main(args: Array[String]) {


    if(args.size < 2) print_usage

    val x = readDoubleMatrix(args(0))
    val y = readBooleanVector(args(1))

    val numTimes = 10
    for (i <- 0 until numTimes) {
      PerformanceTimer.start("GDAbaseline")
      val (phi, mu0, mu1, sigma) = gda(x,y)
      PerformanceTimer.stop("GDAbaseline")
      PerformanceTimer.print("GDAbaseline")
    }
    PerformanceTimer.save("GDAbaseline")
  }

  def gda(x: DoubleMatrix, y: BooleanVector): (Double, DoubleVector, DoubleVector, DoubleMatrix) = {

    /* number of training samples */
    val m = y.length
    //val m = 2

    /* dimensionality of training data */
    val n = x.width

    /* Number of training points must equal number of labeled output points */
    //if (x.h != m) throw new RuntimeException("Bad inputs to GDA")

    /* phi, mu0, mu1, and sigma parameterize the GDA model, where we assume the
    * input features are continuous-valued random variables with a multivariate
    * normal distribution.
    *
    * phi is a scalar, mu0 and mu1 are n dimensional vectors,
    * where n is the width of x, and sigma is an n x n matrix.
    *
    * Our vector and Matrix classes can be preallocated, they will be zeroed
    */

    var y_ones = 0.0; var y_zeros = 0.0
    var mu0_num = DoubleVector(n); var mu1_num = DoubleVector(n)

    /* This loop calculates all of the needed statistics with a single pass
    through the data.  */
    for (i <- 0 until m) {
      if (y(i) == false) {
        y_zeros += 1
        mu0_num = mu0_num + x(i)
      }
      if (y(i) == true) {
        y_ones += 1
        mu1_num = mu1_num + x(i)
      }
    }

    val phi = 1./ m * y_ones
    val mu0 = mu0_num / y_zeros
    val mu1 = mu1_num / y_ones

    /* calculate covariance matrix sigma */
    /* x(i) is a row vector for us, while it is defined a column vector in the formula */
    var sigma = DoubleMatrix(n, n)
    for (i <- 0 until m) {
      if (y(i) == false) {
        sigma += ((x(i) - mu0).trans).outer(x(i) - mu0)
      }
      else if (y(i) == true) {
        sigma += ((x(i) - mu1).trans).outer(x(i) - mu1)
      }
    }

    //println("GDA parameter calculation finished: ")
    //println("  phi = " + phi)
    //println("  mu0 = " + mu0.pprint)
    //println("  mu1 = " + mu1.pprint)
    //println("  sigma = " + sigma.pprint)


    (phi, mu0, mu1, sigma)
  }

}