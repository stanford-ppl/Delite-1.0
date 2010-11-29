package ppl.apps.ml.ica

import ppl.delite.core.appinclude._
import ppl.delite.dsl.optiml.io.MLInputReader
import ppl.delite.dsl.optiml._
import ppl.delite.dsl.optiml.Precursors._
import ppl.delite.metrics._
import ppl.delite.dsl.primitive.DeliteDouble
import ppl.delite.core.{Delite, DeliteApplication}

/**
 * Created by IntelliJ IDEA.
 * User: Anand Atreya
 * Date: Jun 30, 2010
 * Time: 9:51:32 PM
 * To change this template use File | Settings | File Templates.
 */

object ICA extends DeliteApplication {
  def print_usage = {
    println("Usage: ICA <data file>")
    exit(-1)
  }

  def run(args: Array[String]) = {
    if (args.length < 1) print_usage

    Delite.init = true

    val mix = MLInputReader.read(args(0))

    // Stochastic gradient descent annealing schedule
    val anneal = Vector[Double](0.1, 0.1, 0.1, 0.05, 0.05, 0.05, 0.02, 0.02, 0.01, 0.01, 0.005, 0.005, 0.002, 0.002, 0.001, 0.001)

    // Initialize parameters
    val n = mix.numCols
    val m = mix.numRows

    var W = Matrix.identity(n) // initialize output matrix

    Delite.init = false

    PerformanceTimer.start("ICA")

    // Iterate through the annealing schedule
    for (iter <- 0 until anneal.length) {
      println("Annealing schedule: " + (iter + 1) + " of " + anneal.length)
      // Randomly iterate through the samples running stochastic gradient descent
      val rowIndices = randPerm(m)
      for (i <- 0 until rowIndices.length) {
        val rowIndex = rowIndices(i)        // TODO: UNSAFE APPLY - NEEDS TO BE FIXED
        // Perform the ICA stochastic gradient descent update
        val row = mix.sliceRows(rowIndex,rowIndex+1)
        val one = Matrix.ones(n,1)
        W = W + anneal(iter) * ((one-2*one dot (one+(-W*row.trans).exp).reciprocal)*row + W.trans.inv)
      }
    }

    PerformanceTimer.stop("ICA")
    PerformanceTimer.print("ICA")
    PerformanceTimer.save("ICA")
  }

  def randPerm(size: Int) : Vector[Int] = {
    randPerm(Vector.range(0, size))
  }

  def randPerm(values: Vector[Int]) : Vector[Int] = {
    val randValues = Vector.rand(values.length)
    for (i <- (values.length - 1) until 0 by -1) {
      val swapIndex = (randValues(i) * (i+1)).intValue
      if (swapIndex != i) {
        val temp = values(swapIndex)
        values(swapIndex) = values(i)
        values(i) = temp
      }
    }
    values
  }
}