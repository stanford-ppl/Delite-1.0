package ppl.apps.robotics.gradient2

import ppl.delite.dsl.optiml._
import ppl.delite.dsl.optiml.appinclude._

// single-channel image
class GrayscaleImage extends Image[Int] {
  import GrayscaleImage._

  def bitwiseOrDownsample(): GrayscaleImage = {
    downsample(2, 2) { slice =>
      slice(0, 0) | slice(1, 0) | slice(0, 1) | slice(1, 1)
    }
  }

  // Compute Scharr gradients
  def gradients(polar: Boolean = false): (Matrix[Float], Matrix[Float]) = {
    //Find X and Y gradients
    val x = convolve(scharrXkernel)
    val y = convolve(scharrYkernel)
    if (polar) cartToPolar(x, y) else (x, y)
  }

  def convolve(kernel: Matrix[Int]): GrayscaleImage = {
    windowedFilter(data, kernel.numRows, kernel.numCols) { slice =>
      (slice dot kernel).sum
    }
  }
}

object GrayscaleImage extends Image[Int] {
  val scharrYkernel = Matrix[Int](Vector[Int](-3, -10, -3), Vector[Int](0, 0, 0), Vector[Int](3, 10, 3))
  val scharrXkernel = scharrYkernel.trans

  def cartToPolar(x: GrayscaleImage, y: GrayscaleImage): (Matrix[Float], Matrix[Float]) = {
    val mag = x.data.zipWith(y.data, (a, b) => math.sqrt(a*a + b*b).asInstanceOf[Float])
    val phase = x.data.zipWith(y.data, (a, b) => (math.atan2(b, a)*180/math.Pi).asInstanceOf[Float]).mmap(a => if (a < 0) a + 360 else a)
    (mag, phase)
  }
}