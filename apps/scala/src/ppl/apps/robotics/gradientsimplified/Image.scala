package ppl.apps.robotics.gradientsimplified

import ppl.delite.dsl.optiml._
import ppl.delite.dsl.optiml.appinclude._
import java.io.{BufferedReader, FileReader}

// single-channel image
class Image(val rows: Int, val cols: Int, val data: Matrix[Int]) {
  def this(rows: Int, cols: Int) = {
    this(rows, cols, Matrix[Int](rows, cols))
  }

  def this(data: Matrix[Int]) = {
    this(data.numRows, data.numCols, data)
  }

  def getRow(row: Int) = {
    data.getRow(row)
  }

  def downsample(): Image = {
    val downsampled = new Image(rows / 2, cols / 2)

    for (i <- 0 until downsampled.rows) {
      for (j <- 0 until downsampled.cols) {
        downsampled.data(i, j) = data(2*i, 2*j) | data(2*i + 1, 2*j) | data(2*i, 2*j + 1) | data(2*i + 1, 2*j + 1)
      }
    }
    downsampled
  }

  val scharrYkernel = Matrix[Int](Vector[Int](-3, -10, -3), Vector[Int](0, 0, 0), Vector[Int](3, 10, 3))
  val scharrXkernel = scharrYkernel.trans

  def scharr(): (Image, Image) = {
    (conv3x3(scharrXkernel), conv3x3(scharrYkernel))
  }

  def conv3x3(kernel: Matrix[Int]) = {
    val filtered = new Image(rows, cols)
    for (i <- 1 until rows - 1) {
      for (j <- 1 until cols - 1) {
        // Not using Matrix.mdot at the moment due to overhead and lack of simple 2d slicing
        filtered.data(i, j) =
                kernel(0, 0) * data(i - 1, j - 1) + kernel(0, 1) * data(i - 1, j) + kernel(0, 2) * data(i - 1, j + 1) +
                kernel(1, 0) * data(i    , j - 1) + kernel(1, 1) * data(i    , j) + kernel(1, 2) * data(i    , j + 1) +
                kernel(2, 0) * data(i + 1, j - 1) + kernel(2, 1) * data(i + 1, j) + kernel(2, 2) * data(i + 1, j + 1)
      }
    }
    filtered
  }
}

object Image {
  def load(filename: String): Image = {
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

    new Image(x)
  }
}