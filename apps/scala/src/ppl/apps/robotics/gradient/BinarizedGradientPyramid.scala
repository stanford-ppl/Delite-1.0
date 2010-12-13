package ppl.apps.robotics.gradient

import collection.mutable.ArrayBuffer

/**
 * Created by IntelliJ IDEA.
 * User: Anand Atreya
 * Date: Oct 17, 2010
 * Time: 10:36:16 PM
 * To change this template use File | Settings | File Templates.
 */

class BinarizedGradientPyramid(gradientImage: Image, val start_level: Int, val levels: Int) {
  val pyramid = new ArrayBuffer[Image]()

  var crt = gradientImage
  for (i <- 0 until start_level + levels) {
    if (i >= start_level) {
      pyramid += crt
    }
    if (i != (start_level + levels - 1)) {
      crt = crt.downsample()
    }
  }

  def getIndex(index: Int) = pyramid(index - start_level)
}