package ppl.apps.robotics.gradient2

import ppl.dsl.optiml._
import ppl.delite.framework.DeliteApplication

object gradient extends DeliteApplication with OptiMLExp {
  def run() = {

    val image = ImageReader.readGrayscaleImage(args(0))
    val templateFiles = Vector[String]()
    new java.io.File(args(1)).getCanonicalFile.listFiles.map{
      file => templateFiles += file.getPath()
    }

    val bigg = BinarizedGradientGrid(templateFiles)

    tic
    var imgs = 0
    while (imgs < 3) {
      bigg.detectAllObjects(image)
      imgs += 1
    }
    toc
  }
}
