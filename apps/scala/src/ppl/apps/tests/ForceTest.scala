package ppl.apps.tests

import ppl.delite.core.DeliteApplication
import ppl.delite.dsl.optiml.Vector
import ppl.delite.metrics.PerformanceTimer
import ppl.apps.ml.baseline.collection.DoubleVector
import ppl.delite.core.appinclude._

/**
 * Author: Kevin J. Brown
 * Date: Jul 7, 2010
 * Time: 5:10:16 PM
 *
 * Pervasive Parallelism Laboratory (PPL)
 * Stanford University
 */

object ForceTest extends DeliteApplication {
  @volatile var marker0 = 0
  @volatile var marker1 = 0
  @volatile var marker2 = 0
  @volatile var marker3 = 0

  @volatile var vector: Vector[Double] = null
  @volatile var baseVector: DoubleVector = null

  def run(args: Array[String]) {
    /*val*/ vector = Vector.zeros(100)
    /*val*/ baseVector = DoubleVector(100)
    val times = 1000000000

    val numTimes = 5
    for (j <- 0 until numTimes) {
//      PerformanceTimer.start("forcing in delite op " + j)
//      val accV = Vector.range(0, 1).map(k => {
//        var i = 0
//        var acc = 0
//        while (i < times) {
//          acc += vector.length
//          i += 1
//        }
//        acc
//      })
//      accV.force
//      val accD = accV(0)
//      PerformanceTimer.stop("forcing in delite op " + j)
//      PerformanceTimer.print("forcing in delite op " + j)
      val accD = 0

      PerformanceTimer.start("forcing in delite main " + j)
      marker0 = 1
      var k = 0
      var accM = 0
      while (k < times) {
        accM += vector.length
        k += 1
      }
      marker1 = 1
      PerformanceTimer.stop("forcing in delite main " + j)
      PerformanceTimer.print("forcing in delite main " + j)

      PerformanceTimer.start("forcing baseline " + j)
      marker2 = 1
      var i = 0
      var accB = 0
      while (i < times) {
        accB += baseVector.length
        i += 1
      }
      marker3 = 1
      PerformanceTimer.stop("forcing baseline " + j)
      PerformanceTimer.print("forcing baseline " + j)
      println((accD == times*100) + " " + (accM == times*100) + " " + (accB == times*100))
    }
  }

}
