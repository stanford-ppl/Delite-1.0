package ppl.apps.ml.autoencoder

import ppl.delite.core.appinclude._
import ppl.delite.dsl.optiml.io.MLInputReader
import ppl.delite.dsl.optiml._
import ppl.delite.dsl.optiml.Precursors._
import ppl.delite.metrics._
import ppl.delite.core.{Delite, DeliteApplication}
import ppl.delite.dsl.primitive.{DeliteFloat, DeliteDouble}

/**
 * Anand Atreya
 * Based off research app by + discussions with Andrew Saxe
 */

object autoencoder extends DeliteApplication {
  def print_usage = {
    println("Usage: autoencoder <data file> <numInputs> <numExamples> <numHiddenUnits>")
    exit(-1)
  }

  def run(args: Array[String]) = {

    if (args.length < 4) print_usage

    Delite.init = true

//    val numHiddenUnits = 20
//    val numInputs = 40
//    val numExamples = 100

    val numHiddenUnits = args(3).toInt
    val numInputs = args(1).toInt
    val numExamples = args(2).toInt
    val x = if (args(0) != "RAND") MLInputReader.read(args(0)) else Matrix.randn(numInputs, numExamples)

    val alpha = 0.01

    val numTimes = 3
    Delite.init = false

    for (i <- 0 until numTimes) {

      PerformanceTimer.start("autoencoder")

      var W1 = Matrix.randn(numHiddenUnits, numInputs)
      var W2 = Matrix.randn(numInputs, numHiddenUnits)

      var b1 = Vector.randn(numHiddenUnits).mtrans  // TODO: Allow specification of row/column in initialization call
      var b2 = Vector.randn(numInputs).mtrans

      for (i <- 0 until 1000) {
        PerformanceTimerAggregate.start("1-1")
        val z2 = W1 * x + b1.repmat(1, numExamples)
        PerformanceTimerAggregate.stop("1-1")
        PerformanceTimerAggregate.start("1-2")
        // TODO: Think about the best way to encode these types of ops.  Should every common math op be included in OptiML?
        val a2 = z2.map { v => math.tanh(v)}
        PerformanceTimerAggregate.stop("1-2")

        PerformanceTimerAggregate.start("1-3")
        val z3 = W2 * a2 + b2.repmat(1, numExamples)
        PerformanceTimerAggregate.stop("1-3")

//        val error = fronorm(z3 - x) / numExamples
//        println(error)  // Temporary

        PerformanceTimerAggregate.start("2")
        val delta3 = (z3 - x) / numExamples
        val delta2 = (W2.trans * delta3) dot (1 - (a2 dot a2))
        PerformanceTimerAggregate.stop("2")

        PerformanceTimerAggregate.start("3")
        val grad_W2 = delta3 * a2.trans
        val grad_W1 = delta2 * x.trans

        val grad_b2 = delta3.sumRow
        val grad_b1 = delta2.sumRow
        PerformanceTimerAggregate.stop("3")

        PerformanceTimerAggregate.start("4")
        W2 = W2 - alpha * grad_W2
        W1 = W1 - alpha * grad_W1

        b2 = b2 - alpha * grad_b2
        b1 = b1 - alpha * grad_b1
        PerformanceTimerAggregate.stop("4")
      }

      PerformanceTimer.stop("autoencoder")
      PerformanceTimer.print("autoencoder")
      PerformanceTimerAggregate.print("1-1")
      PerformanceTimerAggregate.print("1-2")
      PerformanceTimerAggregate.print("1-3")
      PerformanceTimerAggregate.print("2")
      PerformanceTimerAggregate.print("3")
      PerformanceTimerAggregate.print("4")
      if (i != numTimes) PerformanceTimerAggregate.incrementEpoch
    }
    PerformanceTimer.save("autoencoder")
  }

  def printdim(mat: Matrix[Double]) = {
    println(mat.numRows + "x" + mat.numCols)
  }

  // TODO: Move to Matrix!
  def fronorm(mat: Matrix[Double]) = {
    math.sqrt(trace(mat * mat))
  }

  // TODO: Move to Matrix
  def trace(mat: Matrix[Double]) = {
    assert(mat.numRows == mat.numCols)
    var el = 1
    var sum = mat(0, 0)
    while (el < mat.numRows) {
      sum += mat(el, el)
      el += 1
    }
    sum
  }
}