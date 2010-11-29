package ppl.apps.tests

import ppl.delite.core.appinclude._
import ppl.delite.dsl.optiml.io.MLInputReader
import ppl.delite.dsl.optiml._
import ppl.delite.dsl.optiml.Precursors._
import ppl.delite.metrics._
import ppl.delite.core.{Delite, DeliteApplication}

/**
 * User: Anand Atreya
 * Adapted from Hinton's deep autoencoder implementation
 */

object MatMultTest extends DeliteApplication {

  def print_usage = {
    println("Usage: MatMultTest <matrix dimension>")
    exit(-1)
  }

  def run(args: Array[String]) = {

    Delite.init = true
    if (args.length != 1) print_usage
    val n = args(0).toInt
    var a = Matrix.randf(n, n)
    var b = Matrix.randf(n, n)
    var c:Matrix[Float] = null
    
	val numTimes = 10   

    Delite.init = false

    for (i <- 0 until numTimes) {
      PerformanceTimer.start("MatMult")
      for (j <- 0 until 5) {
        c = (a * b)
		    c.force
      }
      PerformanceTimer.stop("MatMult")
      PerformanceTimer.print("MatMult")
    }

    PerformanceTimer.save("MatMult")
  }
}
