package ppl.apps.ml.baseline.kmeans

import ppl.delite.metrics.PerformanceTimer
import ppl.apps.ml.baseline.collection.{MLInputReader, IntVector, DoubleVector, DoubleMatrix}

object kmeans {
  def print_usage = {
    println("Usage: kmeans <input data file> <initmu data file>")
    exit(-1)
  }

  private val tol = 0.001 // tolerance (for convergence)

  def main(args: Array[String]) {
    
    if (args.length < 1) print_usage

    val x = MLInputReader.readDoubleMatrix(args(0))
    val mu = MLInputReader.readDoubleMatrix(args(1))
    val m = x.height
    val dim = x.width
    val k = mu.height

    var oldmu = DoubleMatrix.random(k, dim)

    val num = 10
    for (i <- 0 until num) {
      val mu_input = mu.clone
      val oldmu_input = oldmu.clone
      PerformanceTimer.start("k-meansbaseline")
      val iters = k_means(x, mu_input, oldmu_input)
      PerformanceTimer.stop("k-meansbaseline")
      println("Finished in " + iters + " iterations")
      PerformanceTimer.print("k-meansbaseline")
//      mu_input.pprint
    }
    PerformanceTimer.save("k-meansbaseline")
  }

  def k_means(x: DoubleMatrix, mu: DoubleMatrix, old_mu: DoubleMatrix): Int = {
    val m = x.height
    val dim = x.width
    val k = mu.height
    var oldmu: DoubleMatrix = DoubleMatrix(1,1)  //just temporary
    // Why is old_mu never used?
    var iter = 0

    do {
      iter += 1
      oldmu = mu.clone

      val c = IntVector.indexedFromFunction(m, e => findNearestCluster(x(e), mu))


      // update mu -- move each cluster centroid to the mean of the points assigned to it
      var j = 0
      while (j != k){
        var weightedpoints = DoubleVector(x.width)
        var points = 0
        var i = 0
        while (i != m) {
          var cval : Int = c(i)
          if (cval == j)
          //if (c(i).value == j)
          {
            weightedpoints = weightedpoints + x(i)
            points += 1
          }
          i += 1
        }
        if (points == 0) mu(j) = DoubleVector(x.width)
        else mu(j) = weightedpoints / points
        j += 1
      }

      //println("Convergence sum: " + (mu-oldmu).abs.sum[DeliteDouble])
    } while ((mu-oldmu).abs.sum > tol)
    iter
  }

  //NOTE: this function returns an Int: need to be careful
  def findNearestCluster( x_i: DoubleVector, mu: DoubleMatrix ) : Int = {
     var min_d = Double.PositiveInfinity
     var min_j = -1

     for (j <- 0 until mu.height) {

       val l = x_i.length
       val v =  DoubleVector(l)
       var idx = 0
       while (idx != l) {
         v(idx) = Math.pow(x_i(idx)- mu(j)(idx),2)
         idx += 1
       }

       val dist = v.sum

       if (dist < min_d){
         min_d = dist
         min_j = j
       }
     }

     return min_j
   }
}