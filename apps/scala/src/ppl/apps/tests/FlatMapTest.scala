package ppl.apps.tests

import ppl.delite.core.appinclude._
import ppl.delite.dsl.optiml._
import ppl.delite.metrics._
import ppl.delite.core.DeliteApplication

object FlatMapTest extends DeliteApplication {

  def print_usage = {
    println("Usage: FlatMapTest <collection size> <element loop size>")
    println("Example: FlatMapTest 1024 10000")
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
      val m1 = map(length, loop)
      m1.force
      PerformanceTimer.stop("map")
      PerformanceTimer.print("map")
      PerformanceTimer.start("flatmap")
      val m2 = flatmap(length, loop)
      m2.force
      PerformanceTimer.stop("flatmap")
      PerformanceTimer.print("flatmap")
      for (i <- 0 until m1.length) {
        assert(m1(i) == m2(i))
      }
    }
  }

  def map(length: Int, loop: Int) = {
    Vector.range(0, length).map(i => {
      var work : Double = 0.0
      var j = 0.0
      while (j != loop) {
        work += 3.0*i+j
        work -= 4.3*i+j
        j += 1
      }
      work
    })
  }

  def flatmap(length: Int, loop: Int) = {
    Vector.range(0, length).flatMap(i => {
      var work : Double = 0.0
      var j = 0.0
      while (j != loop) {
        work += 3.0*i+j
        work -= 4.3*i+j
        j += 1
      }
      Vector[Double](work)
    })
  }
}