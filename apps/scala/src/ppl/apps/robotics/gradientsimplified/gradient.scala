package ppl.apps.robotics.gradientsimplified

import ppl.delite.core.appinclude._
import ppl.delite.dsl.optiml._
import ppl.delite.dsl.optiml.Precursors._
import ppl.delite.metrics._
import ppl.delite.core.{Delite, DeliteApplication}
import ppl.delite.dsl.primitive.{DeliteFloat, DeliteDouble}

object gradient extends DeliteApplication {
  def run(args: Array[String]) = {

    Delite.init = true
    val image = GrayscaleImage.load(args(0))
    val templateFiles = Vector[String]()
    new java.io.File(args(1)).getCanonicalFile.listFiles.map{
      file => templateFiles += file.getPath()
    }

    val bigg = BinarizedGradientGrid(templateFiles)
    Delite.init = false

    var numTimes = 10
    for (i <- 0 until numTimes) {
      PerformanceTimer.start("Gradient")
      for (imgs <- 0 until 3)
        bigg.detectAllObjects(image)

      PerformanceTimer.stop("Gradient")
      PerformanceTimer.print("Gradient")
    }
    PerformanceTimer.save("Gradient")
  }
}
