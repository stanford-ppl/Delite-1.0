package ppl.apps.tests.baseline

import ppl.apps.ml.baseline.collection.FloatMatrix
import ppl.delite.metrics.PerformanceTimer

object MatMultTest {

  def print_usage = {
    println("Usage: MatMultTest <matrix dimension>")
    exit(-1)
  }

  def main(args: Array[String]) = {

    if (args.length != 1) print_usage
    val n = args(0).toInt

    var a = FloatMatrix.random(n, n)
    var b = FloatMatrix.random(n, n)

    val numTimes = 10



    for (i <- 0 until numTimes) {
      PerformanceTimer.start("MatMultbaseline")
      for (j <- 0 until 5) {
        (a * b)
      }
      PerformanceTimer.stop("MatMultbaseline")
      PerformanceTimer.print("MatMultbaseline")
    }

    PerformanceTimer.save("MatMultbaseline")
  }
}