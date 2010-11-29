package ppl.apps.ml.kmeans

import ppl.delite.core.appinclude._
import ppl.delite.dsl.optiml.io.MLInputReader
import ppl.delite.dsl.optiml.{Vector, Matrix}
import ppl.delite.dsl.optiml.appinclude._
import ppl.delite.dsl.primitive.{DeliteInt, DeliteDouble}
import ppl.delite.metrics._
import ppl.delite.core.{DeliteFunc, Delite, DeliteApplication}
import ppl.delite.dsl.optiml.specialized.{DoubleVector, DoubleMatrix}

/**
 * Author: Kevin J. Brown
 * Date: Jun 16, 2010
 * Time: 5:10:22 PM
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

object kmeans extends DeliteApplication {
  def print_usage = {
    println("Usage: kmeans <input data file> <initmu data file>")
    exit(-1)
  }
  
  private val tol = 0.001 // tolerance (for convergence)

  def run(args: Array[String]) {
    
    if (args.length < 1) print_usage

    Delite.init = true

    val x = loadMatrix(args(0))
    val mu = loadMatrix(args(1))
    //val mu = Matrix((0::32) { e => x(randomInt(x.numRows)) })
    val oldmu = Matrix.zeros(mu.numRows, x.numCols)

    Delite.init = false

    val num = 10
    for (i <- 0 until num) {
      val mu_input = mu.clone
      val oldmu_input = oldmu.clone
      PerformanceTimer.start("k-means")
      val (iters,mu_out) = k_means(x, mu_input, oldmu_input)
      PerformanceTimer.stop("k-means")
      println("Finished in " + iters + " iterations")
      PerformanceTimer.print("k-means")
//      mu_out.pprint
    }
    PerformanceTimer.save("k-means")
  }

  def k_means(x: Matrix[Double], mu: Matrix[Double], old_mu: Matrix[Double]): (Int, Matrix[Double]) = {
    val m = x.numRows
    val n = x.numCols
    val k = mu.numRows
    var iter = 0

    untilconverged(mu, tol){ mu =>
      iter += 1

      // update c -- calculate distances to current centroids
      val c = (0::m){e => findNearestCluster(x(e), mu)}

      // update mu -- move each cluster centroid to the mean of the points assigned to it
      (0::k).mforeach(j => {
        val weightedpoints = Vector.zeros(n)
        var points = 0
        for (i <- 0 until m){
          if (c(i) == j){
            weightedpoints += x(i)
            points += 1
          }
        }
        if (points == 0) mu(j) = Vector.zeros(n)
        else mu(j) = weightedpoints / points
        // TODO: fix plugin issue that causes deadlock when registering the mutable dep
      })().force

      mu
    }
    (iter,mu)
  }

  //NOTE: this function returns an Int: need to be careful
  def findNearestCluster( x_i: Vector[Double], mu: Matrix[Double] ) : Int = {
    var min_d = Double.PositiveInfinity
    var min_j = -1

    var j = 0
    while( j < mu._numRows ){
      //val dist : Double = sumd_noplugin(0, x_i._length){ e => Math.pow(x_i(e)-mu(j,e),2) }
      val dist : Double = sumd_noplugin(0, x_i._length){ e => (x_i(e)-mu(j,e))*(x_i(e)-mu(j,e)) }
      if (dist < min_d){
        min_d = dist
        min_j = j
      }
      j += 1
    }

    return min_j
  }
}
