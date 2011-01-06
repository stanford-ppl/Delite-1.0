package ppl.apps.robotics.gradient

import ppl.delite.core.appinclude._
import ppl.delite.dsl.optiml._
import ppl.delite.dsl.optiml.Precursors._
import ppl.delite.metrics._
import ppl.delite.core.{Delite, DeliteApplication}
import ppl.delite.dsl.primitive.{DeliteFloat, DeliteDouble}

//import ros._
//import ros.communication._
//
//import ros.pkg.roscpp_tutorials.srv.TwoInts

object runTest extends DeliteApplication {
  def run(args: Array[String]) = {

    Delite.init = true
    val image = Image.load(args(0))
    val templateFiles = Vector[String]()
    new java.io.File(args(1)).getCanonicalFile.listFiles.map{
      file => templateFiles += file.getPath()
    }

    val bigg = new BinarizedGradientGrid(templateFiles)
    Delite.init = false

    var numTimes = 10
    for (i <- 0 until numTimes) {
      PerformanceTimer.start("Gradient")
      bigg.detectMain(image)
      PerformanceTimer.stop("Gradient")

      PerformanceTimerAggregate.print("computeGradients")
      PerformanceTimerAggregate.print("pyramid")
      PerformanceTimerAggregate.print("detectLoop")
      PerformanceTimerAggregate.print("detect2")
      PerformanceTimerAggregate.print("detect3")
      PerformanceTimerAggregate.print("nonMaxSuppress")
      PerformanceTimer.print("Gradient")
      if (i != numTimes) PerformanceTimerAggregate.incrementEpoch
    }
    PerformanceTimerAggregate.save("computeGradients")
    PerformanceTimerAggregate.save("pyramid")
    PerformanceTimerAggregate.save("detectLoop")
    PerformanceTimerAggregate.save("detect2")
    PerformanceTimerAggregate.save("detect3")
    PerformanceTimerAggregate.save("nonMaxSuppress")
    PerformanceTimer.save("Gradient")
  }
}
