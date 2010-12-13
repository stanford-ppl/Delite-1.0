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
    val cokeImage = Image.load("cokeImage.dat")
    
    val bigg = new BinarizedGradientGrid(Array("coke2-40.txt", "odwalla-orange2-40.txt"))

    Delite.init = false

    //    val (x, y) = cokeImage.scharr()
    //    x.data.pprint
    //    y.data.pprint


    var numTimes = 10
    for (i <- 0 until numTimes) {
      PerformanceTimer.start("Gradient")
      bigg.detectMain(cokeImage)
      PerformanceTimer.stop("Gradient")
      PerformanceTimer.print("Gradient")
    }
    PerformanceTimer.save("Gradient")
  }
}