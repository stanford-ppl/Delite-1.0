package ppl.apps.tests

import ppl.delite.core.appinclude._
import ppl.delite.dsl.optiml.appinclude._
import ppl.delite.dsl.optiml._
import ppl.delite.metrics._
import ppl.delite.core.DeliteApplication
import ppl.delite.dsl.primitive.DeliteInt

/**
 * Author: Kevin J. Brown
 * Date: Jun 26, 2010
 * Time: 7:46:25 PM
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

object ReduceTest extends DeliteApplication {

  def print_usage = {
    println("Usage: ReduceTest <collection size> <element loop size>")
    println("Example: ReduceTest 1024 10000")
    exit(-1)
  }

  def run(args: Array[String]) {
    if (args.length != 2) print_usage
    val length = args(0).toInt
    val loop = args(1).toInt
    assert(length > 0 && loop > 0)
    val numTimes = 5

    for (i <- 0 until numTimes) {
      PerformanceTimer.start("reduce")
      reduce(length, loop).force
      PerformanceTimer.stop("reduce")
      PerformanceTimer.print("reduce")
    }
  }

  def reduce(length: Int, loop: Int) = {
    Vector.range(0, length).reduce[DeliteInt]((i,j) => {
      var work : Double = 0.0
      var j = 0.0
      while (j != loop) {
        work += 3.0*i+j
        work -= 3.0*i+j
        j += 1
      }
      work.toInt
    })
  }
}