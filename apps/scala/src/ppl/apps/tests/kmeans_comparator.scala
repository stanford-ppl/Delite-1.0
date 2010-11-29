package ppl.apps.tests

import ppl.delite.dsl.optiml.Matrix
import ppl.apps.ml.kmeans.kmeans
import ppl.apps.ml.kmeans.kmeans_be
import ppl.delite.dsl.optiml.appinclude._
import ppl.delite.dsl.optiml.io.MLInputReader
import ppl.delite.core.{DeliteApplication, Delite}
import ppl.delite.core.appinclude._
import ppl.delite.dsl.primitive.DeliteDouble

/* Description
 *
 * author: Arvind Sujeeth (asujeeth@stanford.edu)
 * last modified: Jul 20, 2010
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 *
 */

object kmeans_comparator extends DeliteApplication {
  def print_usage = {
    println("Usage: kmeans_comparator <input data file> <initmu data file>")
    exit(-1)
  }

  val diff : (Matrix[Double], Matrix[Double]) => Double = (m1,m2) => Math.sqrt((m1-m2).map(e => Math.pow(e, 2)).sum[DeliteDouble])

  def run(args: Array[String]){
    if (args.length < 2) print_usage

    Delite.init = true
    val x = MLInputReader.read(args(0))
    val mu_input1 = MLInputReader.read(args(1))
    //val mu_input1 = Matrix((0::32) { e => x(randomInt(x.numRows)) })
    Delite.init = false

    val mu_input2 = mu_input1.clone
    val oldmu1 = Matrix.zeros(mu_input1.numRows, x.numCols)
    val oldmu2 = Matrix.zeros(mu_input1.numRows, x.numCols)

    val (iters1,mu1) = kmeans.k_means(x, mu_input1, oldmu1)
    val (iters2,mu2) = kmeans_be.k_means(x, mu_input2, oldmu2)

    println("normal kmeans mu: ")
    mu1.pprint

    println("best effort kmeans mu: ")
    mu2.pprint

    val ans_diff = diff(mu1,mu2)
    val norm_diff = ans_diff / mu1.map(e => Math.abs(e)).sum[DeliteDouble]

    // we could also measure the point-wise percentage differences of the matrix, and report the
    // average % difference and std dev
    val pct_diff = mu1.zipWith(mu2, (a,b) => Math.abs(a - b) / Math.abs(a) )
    val avg_pct_diff = pct_diff.sum[DeliteDouble] / pct_diff.size

    println("normal kmeans took " + iters1 + " iterations")
    println("best effort kmeans took " + iters2 + " iterations")
    println("absolute euclidean difference between final mu: " + ans_diff)
    println("normalized difference: " + norm_diff)
    println("percentage diff matrix: ")
    pct_diff.pprint
    println("average point-wise percentage difference: " + avg_pct_diff)
  }
}