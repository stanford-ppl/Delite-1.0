package ppl.apps.ml.kmeans

import ppl.delite.dsl.optiml.io.MLInputReader
import ppl.delite.metrics.PerformanceTimer
import ppl.delite.core.{Delite, DeliteApplication}
import ppl.delite.dsl.optiml.{Vector, Matrix}
import ppl.delite.dsl.primitive.{DeliteInt, DeliteDouble}
import ppl.delite.core.{DeliteFunc}
import ppl.delite.core.appinclude._
import ppl.delite.core.executor.DeliteGPUThreadPool
import ppl.delite.core.executor._

/**
 * Author: Kevin J. Brown
 * Date: Jun 16, 2010
 * Time: 5:10:22 PM
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

object kmeans_GPU extends DeliteApplication {

  private val randRef = new scala.util.Random(100)

  private val tol = 0.001 // tolerance (for convergence)

  def run(args: Array[String]) {
    Delite.init = true

    val x = MLInputReader.read(args(0))
    val mu = MLInputReader.read(args(1))
	val m = x.numRows
	val dim = x.numCols
	val k = mu.numRows

	/*
  	val m = 500 // number of data points
  	val dim = 200 // data dimensionality
  	val k = 32 // number of clusters
	val x = Matrix.zeros(m, dim).map(e => randRef.nextDouble*10.0)
    var mu = Matrix.zeros(k, dim).mapRow(v => x((randRef.nextDouble*m).asInstanceOf[Int]))
	*/

    // initialize cluster centroids to the value of k random samples from x

    // set oldmu to something other than mu
    var oldmu = Matrix.zeros(k, dim).map(e => randRef.nextDouble*10.0)

    Delite.init = false

    val num = 10
    for (i <- 0 until num) {
      val mu_input = mu.clone

      DeliteGPUThreadPool.threads(0).cleanup_done = false
      DeliteGPUThreadPool.threads(0).cleanup = true
      while(!DeliteGPUThreadPool.threads(0).cleanup_done) {}
      
      PerformanceTimer.start("k-meansgpu")
      val iters = k_means(x, mu_input, oldmu)
      PerformanceTimer.stop("k-meansgpu")

      println("Finished in " + iters + " iterations")
      PerformanceTimer.print("k-meansgpu")
    }
    PerformanceTimer.save("k-meansgpu")
  }

  def k_means(x: Matrix[Double], muu: Matrix[Double], old_mu: Matrix[Double]): Int = {
    var oldmu = old_mu
    var iter = 0

    var mu = muu

	val k = mu.numRows
	val m = x.numRows
	val dim = mu.numCols

    while ((mu-oldmu).abs.sum[DeliteDouble] > tol)
    {
      iter += 1
      oldmu = mu.clone

      // update c -- calculate distances to current centroids
      val c = Vector.range(0, m).mapKM1(e => findNearestCluster(x(e), mu))(x, mu)

      // Above map operation can be changed to below codes
      /*
      var i = 0
      var c = Vector[DeliteInt](m)
      while(i < m) {
        c(i) = findNearestCluster(x.$(i), mu)
        i += 1
      }
      */

      // mu needs to be re-generated to make GPU cache miss
      mu = Matrix.zeros(k, dim)

      // update mu -- move each cluster centroid to the mean of the points assigned to it
      Vector.range(0,k).mapKM2(j => {
      //(Vector.range(0,k).map(j => {
        var weightedpoints = Vector.zeros(x.numCols)
        var points = 0
		var i = 0
        while (i < m){
          var cval : Int = c(i)
          if (cval == j)
          {
            weightedpoints = weightedpoints + x(i)
            points += 1
          }
		  i += 1
        }
        if (points == 0) mu(j) = Vector.zeros(x.numCols)
        else mu(j) = weightedpoints / points
        0
      })(x, mu, c)
      //})).force

      //println("Convergence sum: " + (mu-oldmu).abs.sum[DeliteDouble].value)
      //println("Convergence sum: " + iter)

    }
    iter
  }


  //NOTE: this function returns an Int: need to be careful
  def findNearestCluster( x_i: Vector[Double], mu: Matrix[Double] ) : Int = {
    var min_d = Double.PositiveInfinity
    var min_j = -1

    for (j <- 0 until mu.numRows) {
      val dist = Vector.range(0, x_i.length).map(e => Math.pow(x_i(e)-mu(j)(e),2)).sum[DeliteDouble].value
      if (dist < min_d){
        min_d = dist
        min_j = j
      }
    }

    // Above loop can be replaced with below codes
    /*
    val diff = x_i - mu
    val sums = (diff dot diff).trans.sumCol //sums: Vector[Double](mu.numRows)
    min_j = sums.minIndex.value
    */

     return min_j
   }

  /*
   def calcPoints( x: Matrix[Double], c: Vector[DeliteInt], j: Int ) : Vector[Double] = {
     var weightedpoints: Vector[Double] = Vector(true, x.numCols)
     var points = 0

     for (i <- 0 until x.numRows){
       if (c(i).value == j)
       {
         weightedpoints = (weightedpoints + x(i))
         points += 1
       }
     }
     if (points == 0) Vector.zeros(x.numCols)
     else (weightedpoints / points)
   }
   */

}
