package ppl.apps.tests

import ppl.delite.core.appinclude._
import ppl.delite.dsl.optiml.appinclude._
import ppl.delite.dsl.optiml._
import ppl.delite.metrics._
import ppl.delite.core.DeliteApplication

/**
 * Author: Kevin J. Brown
 * Date: May 12, 2010
 * Time: 7:41:06 PM
 * 
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

object MapTest extends DeliteApplication {

  def print_usage = {
    println("Usage: MapTest <collection size> <element loop size>")
    println("Example: MapTest 1024 10000")
    exit(-1)
  }

  def run(args: Array[String]) {
    if (args.length != 2) print_usage
    val length = args(0).toInt
    val loop = args(1).toInt
    assert(length > 0 && loop > 0)
    val numTimes = 5

    for (i <- 0 until numTimes) {
      PerformanceTimer.start("map")
      map(length, loop).force
      PerformanceTimer.stop("map")
      PerformanceTimer.print("map")
    }
  }

  def map(length: Int, loop: Int) = {
    Vector.range(0, length).map(i => {
      var work : Double = 0.0
      var j = 0.0
      while (j != loop) {
        work += 3.0*i+j
        work -= 3.0*i+j
        j += 1
      }
      work
    })
  }
}