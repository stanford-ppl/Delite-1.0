package ppl.apps.ml.gda

import ppl.delite.core.appinclude._
import ppl.delite.dsl.optiml.{Vector, Matrix}
import ppl.delite.dsl.optiml.appinclude._
import ppl.delite.dsl.optiml.io.MLInputReader._
import ppl.delite.metrics._
import ppl.delite.core.{Config, Delite, DeliteApplication}
import ppl.delite.core.executor.DeliteGPUThreadPool

object GDA_GPU extends DeliteApplication {

  def print_usage = {
    println("Usage: GDA_GPU <input data file> <output label data file>")
    exit(-1)
  }

  def run(args: Array[String]) = {
    if (args.length < 2) print_usage

    Delite.init = true

    val x = read(args(0))
    val y = readVector(args(1)).toBoolean(a => if (a <= 0) false else true)

//    logElapsed("Input Section Complete")
    Delite.init = false

    val num = 10 
    for (i <- 0 until num) {
      DeliteGPUThreadPool.threads(0).cleanup_done = false
      DeliteGPUThreadPool.threads(0).cleanup = true
      while(!DeliteGPUThreadPool.threads(0).cleanup_done) {}
      
      PerformanceTimer.start("GDAGPU")
      val (phi, mu0, mu1, sigma) = gda(x,y)
      sigma.force
      PerformanceTimer.stop("GDAGPU")
      //if(i == (num-1))
      //  sigma.pprint
      PerformanceTimer.print("GDAGPU")
    }

    PerformanceTimer.save("GDAGPU")

    //println("GDA parameter calculation finished: ")
    //println("  phi = " + phi)
    //println("  mu0 = "); mu0.pprint
    //println("  mu1 = "); mu1.pprint
    //println("  sigma = "); sigma.pprint
  }

  def gda(x: Matrix[Double], y: Vector[Boolean]): (Double, Vector[Double], Vector[Double], Matrix[Double]) = {
    val tt1 = System.currentTimeMillis

    /* number of training samples */
    val m = y.length
    //val m = 2

    /* dimensionality of training data */
    val n = x.numCols

    /* Number of training points must equal number of labeled output points */
    //if (x.h != m) throw new RuntimeException("Bad inputs to GDA")

   /* phi, mu0, mu1, and sigma parameterize the GDA model, where we assume the
    * input features are continuous-valued random variables with a multivariate
    * normal distribution.
    *
    * phi is a scalar, mu0 and mu1 are n dimensional vectors,
    * where n is the width of x, and sigma is an n x n matrix.
    */

    var y_ones = 0.0; var y_zeros = 0.0
    var mu0_num = Vector[Double](n); var mu1_num = Vector[Double](n);

    /* This loop calculates all of the needed statistics with a single pass
       through the data.  */
    var i = 0
    while (i < m) {
    //for (i <- 0 until m){
      if (y(i) == false){
        y_zeros += 1
        mu0_num = mu0_num + x.$(i)
      }
      else {
        y_ones += 1
        mu1_num = mu1_num + x.$(i)
      }
      i += 1
    }

    val phi = 1.0 / m * y_ones
    val mu0 = mu0_num / y_zeros
    val mu1 = mu1_num / y_ones

    //mu0.force
    //mu1.force

    //val tt2 = System.currentTimeMillis


    /* calculate covariance matrix sigma */
    /* x(i) is a row vector for us, while it is defined a column vector in the formula */
    var sigma = Matrix[Double](n,n)
    //var mat0 = Matrix[Double](n,n)

    val t1 = System.currentTimeMillis
    i = 0
    while (i < m){
      if (y(i) == false){
        sigma = sigma + ((x.$(i)-mu0).trans).outer(x.$(i)-mu0)
        //sigma += ((x.$(i)-mu0).trans).outer(x.$(i)-mu0)
      }
      else{
        sigma = sigma + ((x.$(i)-mu1).trans).outer(x.$(i)-mu1)
        //sigma += ((x.$(i)-mu1).trans).outer(x.$(i)-mu1)
      }
      i += 1
    }

    sigma.force
    //val sigma0 = sigma + mat0
    //sigma0.force
    //val t2 = System.currentTimeMillis
    //println("Execution time(CPU) is " + (tt2-tt1))
    //println("Execution time(GPU) is " + (t2-t1))
    (phi, mu0, mu1, sigma)
  }
}
