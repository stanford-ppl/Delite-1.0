package ppl.apps.robotics.gradientsimplified

import ppl.delite.dsl.optiml._
import ppl.delite.dsl.optiml.appinclude._

class BinarizedGradientPyramid(gradientImage: GrayscaleImage) {
  val pyramid = Vector[GrayscaleImage]()

  val start_level = 3
  val levels = 1

  val fixedLevelIndex = 3;

  var crt = gradientImage
  for (i <- 0 until start_level + levels) {
    if (i >= start_level) {
      pyramid += crt
    }
    if (i != (start_level + levels - 1)) {
      crt = crt.downsample()
    }
  }
  pyramid.force // Necessary in 1.0 since lifted apply on generic vector isn't implemented

  def getIndex(index: Int) = pyramid.$(index - start_level)
}