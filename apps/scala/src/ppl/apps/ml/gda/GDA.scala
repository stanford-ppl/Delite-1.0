package ppl.apps.ml.gda

import ppl.delite.core.appinclude._
import ppl.delite.dsl.optiml.{Vector, Matrix}
import ppl.delite.dsl.optiml.appinclude._
import ppl.delite.dsl.optiml.io.MLInputReader._
import ppl.delite.metrics._
import ppl.delite.core.{Config, Delite, DeliteApplication}
import ppl.delite.dsl.optiml.specialized.DoubleMatrix

object GDA extends DeliteApplication {

  def print_usage = {
    println("Usage: GDA <input data file> <output label data file>")
    exit(-1)
  }

  def run(args: Array[String]) = {
    if (args.length < 2) print_usage

    Delite.init = true

    val x = loadTrainingSet(args(0))
    val y = loadVector(args(1)).toBoolean(a => if (a <= 0) false else true)

//    logElapsed("Input Section Complete")
    Delite.init = false

    val num = 10
    for (i <- 0 until num) {
      PerformanceTimer.start("GDA")
      val (phi, mu0, mu1, sigma) = gda(x,y)
      sigma.force
      PerformanceTimer.stop("GDA")
      PerformanceTimer.print("GDA")
    }

    PerformanceTimer.save("GDA")
  }

  def gda(x: Matrix[Double], y: Vector[Boolean]): (Double, Vector[Double], Vector[Double], Matrix[Double]) = {
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

    //val (y_zeros, y_ones, mu0_num, mu1_num) = sum(0, m) {
    //  if (y(i) == false){
    //    (1, 0, x(i), 0)
    //  }
    //  else {
    //    (0, 1, 0, x(i))
    //  }
    //}

    for (i <- 0 until m){
      if (y(i) == false){
        y_zeros += 1
        mu0_num = mu0_num + x.$(i)
      }
      else {
        y_ones += 1
        mu1_num = mu1_num + x.$(i)
      }
    }

    val phi = 1./m * y_ones
    val mu0 = mu0_num / y_zeros
    val mu1 = mu1_num / y_ones

    /* calculate covariance matrix sigma */
    /* x(i) is a row vector for us, while it is defined a column vector in the formula */
    
    val sigma = sum(0, m) { i =>
      if (y(i) == false){
        (((x(i)-mu0).trans).outer(x(i)-mu0))
      }
      else{
        (((x(i)-mu1).trans).outer(x(i)-mu1))
      }
    }


    /*
    val sigma = sum(0, m) {
      if (y($) == false){
        ((x($)-mu0).trans).outer(x($)-mu0)
      }
      else{
        ((x($)-mu1).trans).outer(x($)-mu1)
      }
    }
    */

    /*
    var sigma = Matrix[Double](n,n)
    for (i <- 0 until m){
      if (y(i) == false){
        sigma = sigma + ((x.$(i)-mu0).trans).outer(x.$(i)-mu0)
      }
      else{
        sigma = sigma + ((x.$(i)-mu1).trans).outer(x.$(i)-mu1)
      }
    }
    */
    //TODO: fuse the sigma += blah expressions into a macro-op to minimize array allocations
    /*
    val end = if(Config.CPUThreadNumOverride) Config.CPUThreadNum else Runtime.getRuntime.availableProcessors
    val coll = Vector.range(0,end).map(j => {
      var sigma = Matrix[Double](n,n)
      for (i <- m*j/end until m*(j+1)/end) {
        if (y(i) == false) {
          sigma += ((x(i) - mu0).trans).outer(x(i) - mu0)
        }
        else {
          sigma += ((x(i) - mu1).trans).outer(x(i) - mu1)
        }
      }
      sigma
    })

    val sigma = coll.sum[Matrix[Double]]
    */

    //println("GDA parameter calculation finished: ")
    //println("  phi = " + phi)
    //println("  mu0 = "); mu0.pprint
    //println("  mu1 = "); mu1.pprint
    //println("  sigma = "); sigma.pprint
    
    (phi, mu0, mu1, sigma)
  }
}