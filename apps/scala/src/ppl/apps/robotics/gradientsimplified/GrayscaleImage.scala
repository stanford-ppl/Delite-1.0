package ppl.apps.robotics.gradientsimplified

import ppl.delite.core.appinclude._
import ppl.delite.dsl.optiml._
import ppl.delite.dsl.optiml.appinclude._
import ppl.delite.dsl.optiml.Precursors._
import ppl.delite.dsl.primitive._
import java.io.{BufferedReader, FileReader}

// single-channel image
class GrayscaleImage(val rows: Int, val cols: Int, val data: Matrix[Int]) {
  import GrayscaleImage._
  def this(rows: Int, cols: Int) = {
    this(rows, cols, Matrix[Int](rows, cols))
  }

  def this(data: Matrix[Int]) = {
    this(data.numRows, data.numCols, data)
  }

  def getRow(row: Int) = {
    data.getRow(row)
  }

//  def bitwiseOrDownsample(): GrayscaleImage = {
//    val downsampled = new GrayscaleImage(rows / 2, cols / 2)
//
//    for (i <- 0 until downsampled.rows) {
//      for (j <- 0 until downsampled.cols) {
//        downsampled.data(i, j) = data(2*i, 2*j) | data(2*i + 1, 2*j) | data(2*i, 2*j + 1) | data(2*i + 1, 2*j + 1)
//      }
//    }
//    downsampled
//  }

  def bitwiseOrDownsample(): GrayscaleImage = {
    new GrayscaleImage(downsample(2, 2) { slice =>
      slice(0, 0) | slice(1, 0) | slice(0, 1) | slice(1, 1)
    })
  }

  def downsample(rowFactor: Int, colFactor: Int)(block: Matrix[Int] => Int): Matrix[Int] = {
    val output = Matrix[Int](data.numRows / rowFactor, data.numCols / colFactor)
    var row = 0
    while (row < output.numRows) {
      var col = 0
      while (col < output.numCols) {
        val slice = data.slice2d(rowFactor * row, rowFactor * row + rowFactor, colFactor * col, colFactor * col + colFactor)
        slice.force // Necessary in 1.0 due to generic apply
        output(row, col) = block(slice)
        col += 1
      }
      row += 1
    }
    output
  }

  val scharrYkernel = Matrix[Int](Vector[Int](-3, -10, -3), Vector[Int](0, 0, 0), Vector[Int](3, 10, 3))
  val scharrXkernel = scharrYkernel.trans

  // Compute Scharr magnitude and phase in degrees from single channel image
  def gradients(polar: Boolean = false): (Matrix[Float], Matrix[Float]) = {
    //Find X and Y gradients
    val x = convolve(scharrXkernel)
    val y = convolve(scharrYkernel)
    if (polar) cartToPolar(x, y) else (x, y)
  }

  def convolve(kernel: Matrix[Int]): Matrix[Int] = {
    data.windowedFilter(kernel.numRows, kernel.numCols) { slice =>
      (slice dot kernel).sum[DeliteInt]
    }
  }

//  def conv3x3(kernel: Matrix[Int]) = {
//    val filtered = new GrayscaleImage(rows, cols)
//    for (i <- 1 until rows - 1) {
//      for (j <- 1 until cols - 1) {
//        filtered.data(i, j) =
//                kernel(0, 0) * data(i - 1, j - 1) + kernel(0, 1) * data(i - 1, j) + kernel(0, 2) * data(i - 1, j + 1) +
//                kernel(1, 0) * data(i    , j - 1) + kernel(1, 1) * data(i    , j) + kernel(1, 2) * data(i    , j + 1) +
//                kernel(2, 0) * data(i + 1, j - 1) + kernel(2, 1) * data(i + 1, j) + kernel(2, 2) * data(i + 1, j + 1)
//      }
//    }
//    filtered.data
//  }


}

object GrayscaleImage {
  def cartToPolar(x: Matrix[Float], y: Matrix[Float]): (Matrix[Float], Matrix[Float]) = {
    val mag = x.zipWith(y, (a, b) => math.sqrt(a*a + b*b).asInstanceOf[Float])
    val phase = x.zipWith(y, (a, b) => (math.atan2(b, a)*180/math.Pi).asInstanceOf[Float]).mmap(a => if (a < 0) a + 360 else a)
    (mag, phase)
  }

  def load(filename: String): GrayscaleImage = {
    val xfs = new BufferedReader(new FileReader(filename))
    var line = xfs.readLine()
    line = line.trim()
    var ints = line.split("\\s+")
    val x = Matrix[Int](0, ints.length)

    while (line != null){
      val v = Vector[Int](ints.length)
      for (i <- 0 until ints.length){
        v(i) = Integer.parseInt(ints(i))
      }
      x += v

      line = xfs.readLine()
      if (line != null) {
        line = line.trim()
        ints = line.split("\\s+")
      }
    }
    xfs.close()

    new GrayscaleImage(x)
  }
}
