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

object MatMultTest_GPU extends DeliteApplication {

  def print_usage = {
    println("Usage: MatMultTest_GPU <matrix dimension>")
    exit(-1)
  }

  def run(args: Array[String]) = {

    Delite.init = true
    if (args.length != 1) print_usage
    val n = args(0).toInt
    var a = new Array[Matrix[Float]](5)
	var b = new Array[Matrix[Float]](5)
    var c:Matrix[Float] = null
    
	for(i <- 0 until 5) {
		a(i) = Matrix.randf(n,n)
		b(i) = Matrix.randf(n,n)
	}

	val numTimes = 10   

    Delite.init = false

    for (i <- 0 until numTimes) {
      PerformanceTimer.start("MatMultGPU")
      for (j <- 0 until 5) {
        c = (a(j) * b(j))
		c.force
      }
      PerformanceTimer.stop("MatMultGPU")
      PerformanceTimer.print("MatMultGPU")
    }

    PerformanceTimer.save("MatMultGPU")
  }
}
