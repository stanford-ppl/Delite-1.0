package ppl.apps.robotics.gradient2

import ppl.delite.dsl.optiml._
import ppl.delite.dsl.optiml.appinclude._

class BinarizedGradientPyramid(gradientImage: GrayscaleImage) {
  val pyramid = Vector[GrayscaleImage]()

  val start_level = 3
  val levels = 1

  val fixedLevelIndex = 3;

  var crt = gradientImage
  var currentLevel = 0
  while (currentLevel < start_level + levels) {
    if (currentLevel >= start_level) {
      pyramid += crt
    }
    if (currentLevel != (start_level + levels - 1)) {
      crt = crt.downsample()
    }
    currentLevel += 1
  }

  def getIndex(index: Int) = pyramid(index - start_level)
}